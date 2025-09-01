(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-dispute-exists (err u107))
(define-constant skill-web-dev u1)
(define-constant skill-mobile-dev u2)
(define-constant skill-design u3)
(define-constant skill-writing u4)
(define-constant skill-marketing u5)
(define-constant skill-data-science u6)

(define-data-var next-project-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var platform-fee-rate uint u250)

(define-constant err-already-favorited (err u108))
(define-constant err-not-favorited (err u109))

(define-data-var next-favorite-id uint u1)

(define-map projects
  { project-id: uint }
  {
    client: principal,
    freelancer: principal,
    title: (string-ascii 100),
    total-amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map milestones
  { milestone-id: uint }
  {
    project-id: uint,
    amount: uint,
    description: (string-ascii 200),
    status: (string-ascii 20),
    due-date: uint,
    completed-at: (optional uint),
    approved-at: (optional uint)
  }
)

(define-map disputes
  { dispute-id: uint }
  {
    project-id: uint,
    milestone-id: (optional uint),
    initiator: principal,
    reason: (string-ascii 500),
    status: (string-ascii 20),
    created-at: uint,
    resolved-at: (optional uint),
    resolution: (optional (string-ascii 500))
  }
)

(define-map project-escrow
  { project-id: uint }
  { amount: uint }
)

(define-map user-profiles
  { user: principal }
  {
    name: (string-ascii 50),
    reputation: uint,
    total-projects: uint,
    successful-projects: uint,
    is-verified: bool
  }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

(define-public (create-project (freelancer principal) (title (string-ascii 100)) (total-amount uint))
  (let
    (
      (project-id (var-get next-project-id))
    )
    (asserts! (> total-amount u0) err-invalid-amount)
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    (map-set projects
      { project-id: project-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        title: title,
        total-amount: total-amount,
        status: "active",
        created-at: stacks-block-height,
        completed-at: none
      }
    )
    (map-set project-escrow
      { project-id: project-id }
      { amount: total-amount }
    )
    (var-set next-project-id (+ project-id u1))
    (update-user-stats tx-sender)
    (ok project-id)
  )
)

(define-public (create-milestone (project-id uint) (amount uint) (description (string-ascii 200)) (due-date uint))
  (let
    (
      (milestone-id (var-get next-milestone-id))
      (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    )
    (asserts! (is-eq (get client project) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status project) "active") err-invalid-status)
    (asserts! (> amount u0) err-invalid-amount)
    (map-set milestones
      { milestone-id: milestone-id }
      {
        project-id: project-id,
        amount: amount,
        description: description,
        status: "pending",
        due-date: due-date,
        completed-at: none,
        approved-at: none
      }
    )
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (submit-milestone (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) err-not-found))
      (project (unwrap! (map-get? projects { project-id: (get project-id milestone) }) err-not-found))
    )
    (asserts! (is-eq (get freelancer project) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status milestone) "pending") err-invalid-status)
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone {
        status: "submitted",
        completed-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (approve-milestone (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) err-not-found))
      (project (unwrap! (map-get? projects { project-id: (get project-id milestone) }) err-not-found))
      (escrow (unwrap! (map-get? project-escrow { project-id: (get project-id milestone) }) err-not-found))
      (platform-fee (/ (* (get amount milestone) (var-get platform-fee-rate)) u10000))
      (freelancer-payment (- (get amount milestone) platform-fee))
    )
    (asserts! (is-eq (get client project) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status milestone) "submitted") err-invalid-status)
    (asserts! (>= (get amount escrow) (get amount milestone)) err-insufficient-funds)
    (try! (as-contract (stx-transfer? freelancer-payment tx-sender (get freelancer project))))
    (try! (as-contract (stx-transfer? platform-fee tx-sender contract-owner)))
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone {
        status: "approved",
        approved-at: (some stacks-block-height)
      })
    )
    (map-set project-escrow
      { project-id: (get project-id milestone) }
      { amount: (- (get amount escrow) (get amount milestone)) }
    )
    (update-user-balance (get freelancer project) freelancer-payment)
    (ok true)
  )
)

(define-public (create-dispute (project-id uint) (milestone-id (optional uint)) (reason (string-ascii 500)))
  (let
    (
      (dispute-id (var-get next-dispute-id))
      (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    )
    (asserts! (or (is-eq (get client project) tx-sender) (is-eq (get freelancer project) tx-sender)) err-unauthorized)
    (asserts! (is-none (get-active-dispute project-id)) err-dispute-exists)
    (map-set disputes
      { dispute-id: dispute-id }
      {
        project-id: project-id,
        milestone-id: milestone-id,
        initiator: tx-sender,
        reason: reason,
        status: "open",
        created-at: stacks-block-height,
        resolved-at: none,
        resolution: none
      }
    )
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

(define-public (resolve-dispute (dispute-id uint) (resolution (string-ascii 500)) (refund-to-client uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) err-not-found))
      (project (unwrap! (map-get? projects { project-id: (get project-id dispute) }) err-not-found))
      (escrow (unwrap! (map-get? project-escrow { project-id: (get project-id dispute) }) err-not-found))
      (refund-to-freelancer (- (get amount escrow) refund-to-client))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status dispute) "open") err-invalid-status)
    (if (> refund-to-client u0)
      (try! (as-contract (stx-transfer? refund-to-client tx-sender (get client project))))
      true
    )
    (if (> refund-to-freelancer u0)
      (try! (as-contract (stx-transfer? refund-to-freelancer tx-sender (get freelancer project))))
      true
    )
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute {
        status: "resolved",
        resolved-at: (some stacks-block-height),
        resolution: (some resolution)
      })
    )
    (map-set project-escrow
      { project-id: (get project-id dispute) }
      { amount: u0 }
    )
    (ok true)
  )
)

(define-public (complete-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (escrow (unwrap! (map-get? project-escrow { project-id: project-id }) err-not-found))
    )
    (asserts! (is-eq (get client project) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status project) "active") err-invalid-status)
    (if (> (get amount escrow) u0)
      (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get freelancer project))))
      true
    )
    (map-set projects
      { project-id: project-id }
      (merge project {
        status: "completed",
        completed-at: (some stacks-block-height)
      })
    )
    (map-set project-escrow
      { project-id: project-id }
      { amount: u0 }
    )
    (update-user-stats (get client project))
    (update-user-stats (get freelancer project))
    (ok true)
  )
)

(define-public (create-profile (name (string-ascii 50)))
  (begin
    (map-set user-profiles
      { user: tx-sender }
      {
        name: name,
        reputation: u100,
        total-projects: u0,
        successful-projects: u0,
        is-verified: false
      }
    )
    (map-set user-balances
      { user: tx-sender }
      { balance: u0 }
    )
    (ok true)
  )
)

(define-public (verify-user (user principal))
  (let
    (
      (profile (unwrap! (map-get? user-profiles { user: user }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set user-profiles
      { user: user }
      (merge profile { is-verified: true })
    )
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-rate u1000) err-invalid-amount)
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

(define-private (update-user-stats (user principal))
  (let
    (
      (profile (default-to
        { name: "", reputation: u100, total-projects: u0, successful-projects: u0, is-verified: false }
        (map-get? user-profiles { user: user })
      ))
    )
    (map-set user-profiles
      { user: user }
      (merge profile { total-projects: (+ (get total-projects profile) u1) })
    )
  )
)

(define-private (update-user-balance (user principal) (amount uint))
  (let
    (
      (current-balance (default-to { balance: u0 } (map-get? user-balances { user: user })))
    )
    (map-set user-balances
      { user: user }
      { balance: (+ (get balance current-balance) amount) }
    )
  )
)

(define-private (get-active-dispute (project-id uint))
  (let
    (
      (dispute-id u1)
    )
    (map-get? disputes { dispute-id: dispute-id })
  )
)

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones { milestone-id: milestone-id })
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

(define-read-only (get-user-balance (user principal))
  (map-get? user-balances { user: user })
)

(define-read-only (get-project-escrow (project-id uint))
  (map-get? project-escrow { project-id: project-id })
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)


(define-map freelancer-skills
  { freelancer: principal, skill-id: uint }
  { proficiency-level: uint, total-ratings: uint, average-rating: uint }
)

(define-map project-skills
  { project-id: uint }
  { required-skills: (list 5 uint), min-rating: uint }
)

(define-public (register-freelancer-skill (skill-id uint) (proficiency-level uint))
  (begin
    (asserts! (<= skill-id u6) err-invalid-amount)
    (asserts! (and (>= proficiency-level u1) (<= proficiency-level u5)) err-invalid-amount)
    (map-set freelancer-skills
      { freelancer: tx-sender, skill-id: skill-id }
      { proficiency-level: proficiency-level, total-ratings: u0, average-rating: u0 }
    )
    (ok true)
  )
)

(define-public (set-project-skill-requirements (project-id uint) (required-skills (list 5 uint)) (min-rating uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    )
    (asserts! (is-eq (get client project) tx-sender) err-unauthorized)
    (map-set project-skills
      { project-id: project-id }
      { required-skills: required-skills, min-rating: min-rating }
    )
    (ok true)
  )
)

(define-public (rate-freelancer-skill (project-id uint) (skill-id uint) (rating uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (skill-data (unwrap! (map-get? freelancer-skills { freelancer: (get freelancer project), skill-id: skill-id }) err-not-found))
      (new-total (+ (get total-ratings skill-data) u1))
      (new-average (/ (+ (* (get average-rating skill-data) (get total-ratings skill-data)) rating) new-total))
    )
    (asserts! (is-eq (get client project) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status project) "completed") err-invalid-status)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-amount)
    (map-set freelancer-skills
      { freelancer: (get freelancer project), skill-id: skill-id }
      (merge skill-data { total-ratings: new-total, average-rating: new-average })
    )
    (ok true)
  )
)

(define-read-only (get-freelancer-skill (freelancer principal) (skill-id uint))
  (map-get? freelancer-skills { freelancer: freelancer, skill-id: skill-id })
)

(define-read-only (get-project-skill-requirements (project-id uint))
  (map-get? project-skills { project-id: project-id })
)

(define-read-only (check-freelancer-qualification (freelancer principal) (project-id uint))
  (let
    (
      (requirements (map-get? project-skills { project-id: project-id }))
    )
    (match requirements
      some-req (>= (get-freelancer-min-rating freelancer (get required-skills some-req)) (get min-rating some-req))
      true
    )
  )
)

(define-private (get-freelancer-min-rating (freelancer principal) (skills (list 5 uint)))
  (fold check-skill-rating skills u5)
)

(define-private (check-skill-rating (skill-id uint) (min-so-far uint))
  (let
    (
      (skill-data (map-get? freelancer-skills { freelancer: tx-sender, skill-id: skill-id }))
    )
    (match skill-data
      some-skill (if (<= (get average-rating some-skill) min-so-far)
                    (get average-rating some-skill)
                    min-so-far)
      u0
    )
  )
)

(define-data-var next-template-id uint u1)

(define-map project-templates
  { template-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 300),
    category: (string-ascii 50),
    estimated-duration: uint,
    min-budget: uint,
    max-budget: uint,
    required-skills: (list 3 uint),
    min-rating: uint,
    is-active: bool,
    usage-count: uint,
    created-at: uint
  }
)

(define-map template-milestones
  { template-id: uint, milestone-index: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 200),
    percentage: uint,
    estimated-days: uint
  }
)

(define-public (create-template 
  (title (string-ascii 100))
  (description (string-ascii 300))
  (category (string-ascii 50))
  (estimated-duration uint)
  (min-budget uint)
  (max-budget uint)
  (required-skills (list 3 uint))
  (min-rating uint))
  (let
    (
      (template-id (var-get next-template-id))
      (user-profile (unwrap! (map-get? user-profiles { user: tx-sender }) err-not-found))
    )
    (asserts! (get is-verified user-profile) err-unauthorized)
    (asserts! (>= (get reputation user-profile) u150) err-unauthorized)
    (asserts! (and (> min-budget u0) (>= max-budget min-budget)) err-invalid-amount)
    (asserts! (> estimated-duration u0) err-invalid-amount)
    (map-set project-templates
      { template-id: template-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        category: category,
        estimated-duration: estimated-duration,
        min-budget: min-budget,
        max-budget: max-budget,
        required-skills: required-skills,
        min-rating: min-rating,
        is-active: true,
        usage-count: u0,
        created-at: stacks-block-height
      }
    )
    (var-set next-template-id (+ template-id u1))
    (ok template-id)
  )
)

(define-public (add-template-milestone
  (template-id uint)
  (milestone-index uint)
  (title (string-ascii 100))
  (description (string-ascii 200))
  (percentage uint)
  (estimated-days uint))
  (let
    (
      (template (unwrap! (map-get? project-templates { template-id: template-id }) err-not-found))
    )
    (asserts! (is-eq (get creator template) tx-sender) err-unauthorized)
    (asserts! (get is-active template) err-invalid-status)
    (asserts! (and (> percentage u0) (<= percentage u100)) err-invalid-amount)
    (asserts! (> estimated-days u0) err-invalid-amount)
    (map-set template-milestones
      { template-id: template-id, milestone-index: milestone-index }
      {
        title: title,
        description: description,
        percentage: percentage,
        estimated-days: estimated-days
      }
    )
    (ok true)
  )
)

(define-read-only (get-template (template-id uint))
  (map-get? project-templates { template-id: template-id })
)

(define-read-only (get-template-milestone (template-id uint) (milestone-index uint))
  (map-get? template-milestones { template-id: template-id, milestone-index: milestone-index })
)


(define-map user-favorites
  { user: principal, project-id: uint }
  { favorited-at: uint, is-active: bool }
)

(define-map project-favorite-count
  { project-id: uint }
  { count: uint }
)

(define-map user-favorite-list
  { user: principal, favorite-id: uint }
  { project-id: uint, added-at: uint }
)

(define-public (add-to-favorites (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (existing-favorite (map-get? user-favorites { user: tx-sender, project-id: project-id }))
      (favorite-id (var-get next-favorite-id))
      (current-count (default-to { count: u0 } (map-get? project-favorite-count { project-id: project-id })))
    )
    (asserts! (not (is-eq (get freelancer project) tx-sender)) err-unauthorized)
    (asserts! (is-eq (get status project) "active") err-invalid-status)
    (asserts! (is-none existing-favorite) err-already-favorited)
    (map-set user-favorites
      { user: tx-sender, project-id: project-id }
      { favorited-at: stacks-block-height, is-active: true }
    )
    (map-set user-favorite-list
      { user: tx-sender, favorite-id: favorite-id }
      { project-id: project-id, added-at: stacks-block-height }
    )
    (map-set project-favorite-count
      { project-id: project-id }
      { count: (+ (get count current-count) u1) }
    )
    (var-set next-favorite-id (+ favorite-id u1))
    (ok true)
  )
)

(define-public (remove-from-favorites (project-id uint))
  (let
    (
      (existing-favorite (unwrap! (map-get? user-favorites { user: tx-sender, project-id: project-id }) err-not-favorited))
      (current-count (unwrap! (map-get? project-favorite-count { project-id: project-id }) err-not-found))
    )
    (asserts! (get is-active existing-favorite) err-not-favorited)
    (map-set user-favorites
      { user: tx-sender, project-id: project-id }
      (merge existing-favorite { is-active: false })
    )
    (map-set project-favorite-count
      { project-id: project-id }
      { count: (- (get count current-count) u1) }
    )
    (ok true)
  )
)

(define-read-only (is-project-favorited (user principal) (project-id uint))
  (match (map-get? user-favorites { user: user, project-id: project-id })
    some-fav (get is-active some-fav)
    false
  )
)

(define-read-only (get-project-favorite-count (project-id uint))
  (default-to { count: u0 } (map-get? project-favorite-count { project-id: project-id }))
)

(define-read-only (get-user-favorite (user principal) (project-id uint))
  (map-get? user-favorites { user: user, project-id: project-id })
)