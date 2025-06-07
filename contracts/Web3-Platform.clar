(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-dispute-exists (err u107))

(define-data-var next-project-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var platform-fee-rate uint u250)

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
