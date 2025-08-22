;; MolochDAO-style Governance Contract
;; Simple pooled treasury with vote-to-exit functionality

;; Error constants
(define-constant ERR-NOT-MEMBER u100)
(define-constant ERR-NOT-FOUND u101)
(define-constant ERR-ALREADY-VOTED u102)
(define-constant ERR-VOTING-ENDED u103)
(define-constant ERR-VOTING-ACTIVE u104)
(define-constant ERR-INSUFFICIENT-SHARES u105)
(define-constant ERR-UNAUTHORIZED u106)
(define-constant ERR-INVALID-AMOUNT u107)

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Voting period (in blocks)
(define-constant VOTING-PERIOD u1440) ;; ~10 days assuming 10min blocks

;; Data structures
(define-map members 
  principal 
  { shares: uint, joined-at: uint }
)

(define-map proposals 
  uint 
  {
    proposer: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    amount-requested: uint,
    recipient: principal,
    yes-votes: uint,
    no-votes: uint,
    votes-by-shares: uint,
    start-block: uint,
    end-block: uint,
    executed: bool
  }
)

(define-map votes 
  { proposal-id: uint, voter: principal }
  { vote: bool, shares: uint }
)

;; Contract state variables
(define-data-var proposal-counter uint u0)
(define-data-var total-shares uint u0)
(define-data-var guild-bank uint u0)

;; Get member info
(define-read-only (get-member (member principal))
  (map-get? members member)
)

;; Get total shares
(define-read-only (get-total-shares)
  (var-get total-shares)
)

;; Get guild bank balance
(define-read-only (get-guild-bank)
  (var-get guild-bank)
)

;; Get proposal
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

;; Check if member has voted on proposal
(define-read-only (has-voted (proposal-id uint) (member principal))
  (is-some (map-get? votes { proposal-id: proposal-id, voter: member }))
)

;; Join DAO by contributing STX
(define-public (join-dao (stx-amount uint))
  (let 
    (
      (caller tx-sender)
      (current-member (map-get? members caller))
    )
    (asserts! (> stx-amount u0) (err ERR-INVALID-AMOUNT))
    
    ;; Transfer STX to contract
    (try! (stx-transfer? stx-amount caller (as-contract tx-sender)))
    
    ;; Update guild bank
    (var-set guild-bank (+ (var-get guild-bank) stx-amount))
    
    ;; Calculate shares (1:1 ratio for simplicity)
    (let ((new-shares stx-amount))
      (match current-member
        existing-member
        ;; Add to existing shares
        (begin
          (map-set members caller {
            shares: (+ (get shares existing-member) new-shares),
            joined-at: (get joined-at existing-member)
          })
          (var-set total-shares (+ (var-get total-shares) new-shares))
        )
        ;; New member
        (begin
          (map-set members caller {
            shares: new-shares,
            joined-at: block-height
          })
          (var-set total-shares (+ (var-get total-shares) new-shares))
        )
      )
    )
    (ok true)
  )
)

;; Submit a proposal
(define-public (submit-proposal 
  (title (string-utf8 100))
  (description (string-utf8 500))
  (amount-requested uint)
  (recipient principal)
)
  (let 
    (
      (caller tx-sender)
      (member-data (unwrap! (map-get? members caller) (err ERR-NOT-MEMBER)))
      (proposal-id (+ (var-get proposal-counter) u1))
      (start-block block-height)
      (end-block (+ start-block VOTING-PERIOD))
    )
    
    ;; Create proposal
    (map-set proposals proposal-id {
      proposer: caller,
      title: title,
      description: description,
      amount-requested: amount-requested,
      recipient: recipient,
      yes-votes: u0,
      no-votes: u0,
      votes-by-shares: u0,
      start-block: start-block,
      end-block: end-block,
      executed: false
    })
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

;; Vote on proposal
(define-public (vote-on-proposal (proposal-id uint) (support bool))
  (let 
    (
      (caller tx-sender)
      (member-data (unwrap! (map-get? members caller) (err ERR-NOT-MEMBER)))
      (proposal-data (unwrap! (map-get? proposals proposal-id) (err ERR-NOT-FOUND)))
      (member-shares (get shares member-data))
    )
    
    ;; Check if voting period is active
    (asserts! (<= block-height (get end-block proposal-data)) (err ERR-VOTING-ENDED))
    
    ;; Check if already voted
    (asserts! (not (has-voted proposal-id caller)) (err ERR-ALREADY-VOTED))
    
    ;; Record vote
    (map-set votes 
      { proposal-id: proposal-id, voter: caller }
      { vote: support, shares: member-shares }
    )
    
    ;; Update proposal vote counts
    (map-set proposals proposal-id
      (merge proposal-data {
        yes-votes: (if support 
          (+ (get yes-votes proposal-data) u1)
          (get yes-votes proposal-data)
        ),
        no-votes: (if support
          (get no-votes proposal-data)
          (+ (get no-votes proposal-data) u1)
        ),
        votes-by-shares: (+ (get votes-by-shares proposal-data) member-shares)
      })
    )
    
    (ok true)
  )
)

;; Execute proposal (if passed)
(define-public (execute-proposal (proposal-id uint))
  (let 
    (
      (proposal-data (unwrap! (map-get? proposals proposal-id) (err ERR-NOT-FOUND)))
      (total-voted-shares (get votes-by-shares proposal-data))
      (yes-votes (get yes-votes proposal-data))
      (no-votes (get no-votes proposal-data))
    )
    
    ;; Check if voting ended
    (asserts! (> block-height (get end-block proposal-data)) (err ERR-VOTING-ACTIVE))
    
    ;; Check if not already executed
    (asserts! (not (get executed proposal-data)) (err ERR-ALREADY-VOTED))
    
    ;; Check if proposal passed (simple majority of shares voted)
    (asserts! (> yes-votes no-votes) (err ERR-UNAUTHORIZED))
    
    ;; Check if quorum met (at least 10% of total shares voted)
    (asserts! (>= total-voted-shares (/ (var-get total-shares) u10)) (err ERR-UNAUTHORIZED))
    
    ;; Execute the proposal
    (let ((amount (get amount-requested proposal-data)))
      (asserts! (<= amount (var-get guild-bank)) (err ERR-INSUFFICIENT-SHARES))
      
      ;; Transfer funds
      (try! (as-contract (stx-transfer? amount tx-sender (get recipient proposal-data))))
      
      ;; Update guild bank
      (var-set guild-bank (- (var-get guild-bank) amount))
      
      ;; Mark as executed
      (map-set proposals proposal-id
        (merge proposal-data { executed: true })
      )
    )
    
    (ok true)
  )
)

;; Ragequit - Exit DAO and claim proportional treasury
(define-public (ragequit (shares-to-burn uint))
  (let 
    (
      (caller tx-sender)
      (member-data (unwrap! (map-get? members caller) (err ERR-NOT-MEMBER)))
      (member-shares (get shares member-data))
      (total-treasury (var-get guild-bank))
      (total-dao-shares (var-get total-shares))
    )
    
    ;; Check if member has enough shares
    (asserts! (<= shares-to-burn member-shares) (err ERR-INSUFFICIENT-SHARES))
    (asserts! (> shares-to-burn u0) (err ERR-INVALID-AMOUNT))
    
    ;; Calculate proportional payout
    (let 
      (
        (payout (/ (* shares-to-burn total-treasury) total-dao-shares))
      )
      
      ;; Update member shares
      (if (is-eq shares-to-burn member-shares)
        ;; Remove member completely
        (map-delete members caller)
        ;; Reduce member shares
        (map-set members caller {
          shares: (- member-shares shares-to-burn),
          joined-at: (get joined-at member-data)
        })
      )
      
      ;; Update totals
      (var-set total-shares (- total-dao-shares shares-to-burn))
      (var-set guild-bank (- total-treasury payout))
      
      ;; Transfer payout to member
      (try! (as-contract (stx-transfer? payout tx-sender caller)))
      
      (ok payout)
    )
  )
)

;; Get member's voting power percentage
(define-read-only (get-voting-power (member principal))
  (match (map-get? members member)
    member-data 
    (let 
      (
        (member-shares (get shares member-data))
        (total-dao-shares (var-get total-shares))
      )
      (if (is-eq total-dao-shares u0)
        u0
        (/ (* member-shares u10000) total-dao-shares) ;; Percentage * 100 for precision
      )
    )
    u0
  )
)

;; Emergency functions (contract owner only)
(define-public (emergency-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) (err ERR-UNAUTHORIZED))
    ;; Implementation would add pause functionality
    (ok true)
  )
)

;; Initialize contract (can be called once by deployer)
(define-public (initialize)
  (begin
    ;; Contract deployer becomes first member with 1 share
    (map-set members CONTRACT-OWNER {
      shares: u1,
      joined-at: block-height
    })
    (var-set total-shares u1)
    (ok true)
  )
)