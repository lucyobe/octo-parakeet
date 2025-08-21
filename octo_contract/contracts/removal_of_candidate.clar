;; Enhanced Voting Smart Contract with Advanced Features
;; Comprehensive voting system with multiple voting types, delegation, and advanced admin controls

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-voting-started (err u103))
(define-constant err-voting-not-started (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-invalid-candidate (err u106))
(define-constant err-voting-ended (err u107))
(define-constant err-insufficient-stake (err u108))
(define-constant err-unauthorized (err u109))
(define-constant err-invalid-period (err u110))
(define-constant err-delegation-cycle (err u111))
(define-constant err-self-delegation (err u112))
(define-constant err-invalid-proposal (err u113))
(define-constant err-proposal-exists (err u114))
(define-constant err-voting-period-active (err u115))
(define-constant err-minimum-participation (err u116))
(define-constant err-invalid-threshold (err u117))

;; Data Variables
(define-data-var voting-started bool false)
(define-data-var voting-end-block uint u0)
(define-data-var registration-end-block uint u0)
(define-data-var total-candidates uint u0)
(define-data-var total-votes uint u0)
(define-data-var total-weighted-votes uint u0)
(define-data-var minimum-stake uint u1000000) ;; 1 STX minimum
(define-data-var voting-type (string-ascii 20) "simple") ;; simple, weighted, ranked, quadratic
(define-data-var quorum-percentage uint u20) ;; 20% minimum participation
(define-data-var approval-threshold uint u50) ;; 50% approval needed
(define-data-var allow-delegation bool true)
(define-data-var allow-vote-changes bool false)
(define-data-var max-candidates-per-voter uint u5) ;; For ranked voting
(define-data-var election-title (string-ascii 100) "")
(define-data-var election-description (string-ascii 500) "")

;; Data Maps
(define-map candidates 
  { candidate-id: uint } 
  { 
    name: (string-ascii 50),
    description: (string-ascii 200),
    vote-count: uint,
    weighted-vote-count: uint,
    quadratic-vote-count: uint,
    active: bool,
    registration-block: uint,
    platform-url: (optional (string-ascii 200)),
    category: (string-ascii 30)
  })

(define-map candidate-by-name 
  { name: (string-ascii 50) } 
  { candidate-id: uint })

(define-map voters 
  { voter: principal } 
  { 
    voted: bool,
    candidate-id: uint,
    vote-block: uint,
    vote-weight: uint,
    stake-amount: uint,
    ranked-votes: (list 10 uint),
    quadratic-credits-used: uint
  })

(define-map voter-delegations
  { delegator: principal }
  { 
    delegate: principal,
    delegation-block: uint,
    active: bool
  })

(define-map delegate-power
  { delegate: principal }
  { 
    total-delegated-power: uint,
    delegator-count: uint
  })

(define-map admin-roles
  { user: principal }
  {
    role: (string-ascii 20), ;; admin, moderator, observer
    granted-by: principal,
    granted-block: uint,
    active: bool
  })

(define-map voter-registry
  { voter: principal }
  {
    registered: bool,
    registration-block: uint,
    stake-amount: uint,
    reputation-score: uint,
    verified: bool
  })

(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    creation-block: uint,
    voting-start-block: uint,
    voting-end-block: uint,
    yes-votes: uint,
    no-votes: uint,
    status: (string-ascii 20), ;; pending, active, passed, rejected, cancelled
    proposal-type: (string-ascii 20), ;; candidate-addition, rule-change, general
    target-candidate: (optional uint),
    execution-block: (optional uint)
  })

(define-map vote-receipts
  { voter: principal, vote-id: uint }
  {
    candidate-id: uint,
    vote-type: (string-ascii 20),
    vote-value: uint,
    timestamp: uint,
    block-height: uint,
    is-delegated: bool
  })

(define-map election-statistics
  { stat-type: (string-ascii 30) }
  { value: uint })

;; Private Functions
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner))

(define-private (is-admin-or-owner)
  (or (is-contract-owner)
      (is-some (map-get? admin-roles { user: tx-sender }))))

(define-private (has-admin-role (user principal))
  (match (map-get? admin-roles { user: user })
    role-data (get active role-data)
    false))

(define-private (calculate-quadratic-cost (votes uint))
  (* votes votes))

(define-private (is-voting-period-active)
  (and (var-get voting-started)
       (or (is-eq (var-get voting-end-block) u0)
           (< block-height (var-get voting-end-block)))))

(define-private (calculate-vote-weight (voter principal))
  (let ((stake (default-to u0 (get stake-amount (map-get? voter-registry { voter: voter }))))
        (reputation (default-to u0 (get reputation-score (map-get? voter-registry { voter: voter })))))
    (+ u1 (/ stake u1000000) (/ reputation u10))))

;; Read-only Functions
(define-read-only (get-candidate (candidate-id uint))
  (map-get? candidates { candidate-id: candidate-id }))

(define-read-only (get-candidate-by-name (name (string-ascii 50)))
  (match (map-get? candidate-by-name { name: name })
    candidate-data (map-get? candidates { candidate-id: (get candidate-id candidate-data) })
    none))

(define-read-only (get-total-candidates)
  (var-get total-candidates))

(define-read-only (get-total-votes)
  (var-get total-votes))

(define-read-only (get-total-weighted-votes)
  (var-get total-weighted-votes))

(define-read-only (has-voting-started)
  (var-get voting-started))

(define-read-only (get-voting-end-block)
  (var-get voting-end-block))

(define-read-only (has-voted (voter principal))
  (default-to false (get voted (map-get? voters { voter: voter }))))

(define-read-only (get-voter-info (voter principal))
  (map-get? voters { voter: voter }))

(define-read-only (is-admin (user principal))
  (or (is-eq user contract-owner) (has-admin-role user)))

(define-read-only (get-delegation-info (delegator principal))
  (map-get? voter-delegations { delegator: delegator }))

(define-read-only (get-delegate-power-info (delegate principal))
  (map-get? delegate-power { delegate: delegate }))

(define-read-only (get-voter-registration (voter principal))
  (map-get? voter-registry { voter: voter }))

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id }))

(define-read-only (get-election-config)
  {
    voting-type: (var-get voting-type),
    quorum-percentage: (var-get quorum-percentage),
    approval-threshold: (var-get approval-threshold),
    allow-delegation: (var-get allow-delegation),
    allow-vote-changes: (var-get allow-vote-changes),
    minimum-stake: (var-get minimum-stake),
    title: (var-get election-title),
    description: (var-get election-description)
  })

(define-read-only (get-voting-statistics)
  {
    total-candidates: (var-get total-candidates),
    total-votes: (var-get total-votes),
    total-weighted-votes: (var-get total-weighted-votes),
    participation-rate: (if (> (var-get total-votes) u0)
                          (/ (* (var-get total-votes) u100) (var-get total-votes))
                          u0),
    voting-started: (var-get voting-started),
    blocks-remaining: (if (and (var-get voting-started) (> (var-get voting-end-block) u0))
                        (if (> (var-get voting-end-block) block-height)
                          (- (var-get voting-end-block) block-height)
                          u0)
                        u0)
  })

;; Public Functions

;; Configuration Functions (Admin Only)
(define-public (set-election-config 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (voting-type-new (string-ascii 20))
  (quorum uint)
  (threshold uint))
  (begin
    (asserts! (is-admin-or-owner) err-unauthorized)
    (asserts! (not (var-get voting-started)) err-voting-started)
    (asserts! (and (>= quorum u1) (<= quorum u100)) err-invalid-threshold)
    (asserts! (and (>= threshold u1) (<= threshold u100)) err-invalid-threshold)
    
    (var-set election-title title)
    (var-set election-description description)
    (var-set voting-type voting-type-new)
    (var-set quorum-percentage quorum)
    (var-set approval-threshold threshold)
    (ok true)))

(define-public (set-voting-period (start-block uint) (end-block uint))
  (begin
    (asserts! (is-admin-or-owner) err-unauthorized)
    (asserts! (not (var-get voting-started)) err-voting-started)
    (asserts! (> end-block start-block) err-invalid-period)
    (asserts! (> start-block block-height) err-invalid-period)
    
    (var-set voting-end-block end-block)
    (ok true)))

(define-public (set-registration-period (end-block uint))
  (begin
    (asserts! (is-admin-or-owner) err-unauthorized)
    (asserts! (not (var-get voting-started)) err-voting-started)
    (asserts! (> end-block block-height) err-invalid-period)
    
    (var-set registration-end-block end-block)
    (ok true)))

;; Admin Role Management
(define-public (grant-admin-role (user principal) (role (string-ascii 20)))
  (begin
    (asserts! (is-contract-owner) err-owner-only)
    (map-set admin-roles 
      { user: user }
      {
        role: role,
        granted-by: tx-sender,
        granted-block: block-height,
        active: true
      })
    (ok true)))

(define-public (revoke-admin-role (user principal))
  (begin
    (asserts! (is-contract-owner) err-owner-only)
    (match (map-get? admin-roles { user: user })
      role-data
      (begin
        (map-set admin-roles 
          { user: user }
          (merge role-data { active: false }))
        (ok true))
      (ok false))))

;; Voter Registration
(define-public (register-voter (stake-amount uint))
  (begin
    (asserts! (>= stake-amount (var-get minimum-stake)) err-insufficient-stake)
    (asserts! (or (is-eq (var-get registration-end-block) u0)
                  (< block-height (var-get registration-end-block))) err-voting-started)
    
    ;; In a real implementation, you'd want to escrow the STX here
    (map-set voter-registry
      { voter: tx-sender }
      {
        registered: true,
        registration-block: block-height,
        stake-amount: stake-amount,
        reputation-score: u100,
        verified: false
      })
    (ok true)))

(define-public (verify-voter (voter principal))
  (begin
    (asserts! (is-admin-or-owner) err-unauthorized)
    (match (map-get? voter-registry { voter: voter })
      registration-data
      (begin
        (map-set voter-registry
          { voter: voter }
          (merge registration-data { verified: true }))
        (ok true))
      err-not-found)))

;; Delegation Functions
(define-public (delegate-vote (delegate principal))
  (begin
    (asserts! (var-get allow-delegation) err-unauthorized)
    (asserts! (not (is-eq tx-sender delegate)) err-self-delegation)
    (asserts! (is-voting-period-active) err-voting-not-started)
    
    ;; Check for delegation cycles (simplified check)
    (asserts! (is-none (get-delegation-info delegate)) err-delegation-cycle)
    
    (let ((delegator-weight (calculate-vote-weight tx-sender)))
      ;; Set delegation
      (map-set voter-delegations
        { delegator: tx-sender }
        {
          delegate: delegate,
          delegation-block: block-height,
          active: true
        })
      
      ;; Update delegate power
      (map-set delegate-power
        { delegate: delegate }
        {
          total-delegated-power: (+ delegator-weight 
            (default-to u0 (get total-delegated-power (map-get? delegate-power { delegate: delegate })))),
          delegator-count: (+ u1 
            (default-to u0 (get delegator-count (map-get? delegate-power { delegate: delegate }))))
        })
      (ok true))))

(define-public (revoke-delegation)
  (begin
    (match (map-get? voter-delegations { delegator: tx-sender })
      delegation-data
      (let ((delegate (get delegate delegation-data))
            (delegator-weight (calculate-vote-weight tx-sender)))
        ;; Remove delegation
        (map-delete voter-delegations { delegator: tx-sender })
        
        ;; Update delegate power
        (match (map-get? delegate-power { delegate: delegate })
          power-data
          (map-set delegate-power
            { delegate: delegate }
            {
              total-delegated-power: (- (get total-delegated-power power-data) delegator-weight),
              delegator-count: (- (get delegator-count power-data) u1)
            })
          true)
        (ok true))
      err-not-found)))

;; Enhanced Candidate Management
(define-public (add-candidate-advanced 
  (name (string-ascii 50)) 
  (description (string-ascii 200))
  (platform-url (optional (string-ascii 200)))
  (category (string-ascii 30)))
  (begin
    (asserts! (is-admin-or-owner) err-unauthorized)
    (asserts! (not (var-get voting-started)) err-voting-started)
    (asserts! (is-none (map-get? candidate-by-name { name: name })) err-already-exists)
    
    (let ((candidate-id (+ (var-get total-candidates) u1)))
      (map-set candidates 
        { candidate-id: candidate-id }
        {
          name: name,
          description: description,
          vote-count: u0,
          weighted-vote-count: u0,
          quadratic-vote-count: u0,
          active: true,
          registration-block: block-height,
          platform-url: platform-url,
          category: category
        })
      (map-set candidate-by-name 
        { name: name }
        { candidate-id: candidate-id })
      (var-set total-candidates candidate-id)
      (ok candidate-id))))

;; Proposal System
(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (proposal-type (string-ascii 20))
  (target-candidate (optional uint)))
  (begin
    (asserts! (is-some (map-get? voter-registry { voter: tx-sender })) err-unauthorized)
    
    (let ((proposal-id (+ (default-to u0 (get value (map-get? election-statistics { stat-type: "total-proposals" }))) u1)))
      (map-set proposals
        { proposal-id: proposal-id }
        {
          title: title,
          description: description,
          proposer: tx-sender,
          creation-block: block-height,
          voting-start-block: (+ block-height u144), ;; 1 day delay
          voting-end-block: (+ block-height u1008), ;; 1 week voting period
          yes-votes: u0,
          no-votes: u0,
          status: "pending",
          proposal-type: proposal-type,
          target-candidate: target-candidate,
          execution-block: none
        })
      
      ;; Update proposal count
      (map-set election-statistics
        { stat-type: "total-proposals" }
        { value: proposal-id })
      (ok proposal-id))))

;; Advanced Voting Functions
(define-public (vote-simple (candidate-id uint))
  (begin
    (asserts! (is-voting-period-active) err-voting-not-started)
    (asserts! (is-eq (var-get voting-type) "simple") err-unauthorized)
    (asserts! (not (has-voted tx-sender)) err-already-voted)
    
    (match (map-get? candidates { candidate-id: candidate-id })
      candidate-data
      (begin
        (asserts! (get active candidate-data) err-invalid-candidate)
        
        (let ((vote-weight (calculate-vote-weight tx-sender)))
          ;; Record vote
          (map-set voters 
            { voter: tx-sender }
            {
              voted: true,
              candidate-id: candidate-id,
              vote-block: block-height,
              vote-weight: vote-weight,
              stake-amount: (default-to u0 (get stake-amount (map-get? voter-registry { voter: tx-sender }))),
              ranked-votes: (list),
              quadratic-credits-used: u0
            })
          
          ;; Update candidate counts
          (map-set candidates 
            { candidate-id: candidate-id }
            (merge candidate-data { 
              vote-count: (+ (get vote-count candidate-data) u1),
              weighted-vote-count: (+ (get weighted-vote-count candidate-data) vote-weight)
            }))
          
          ;; Update totals
          (var-set total-votes (+ (var-get total-votes) u1))
          (var-set total-weighted-votes (+ (var-get total-weighted-votes) vote-weight))
          (ok true)))
      err-invalid-candidate)))

(define-public (vote-ranked (candidate-rankings (list 10 uint)))
  (begin
    (asserts! (is-voting-period-active) err-voting-not-started)
    (asserts! (is-eq (var-get voting-type) "ranked") err-unauthorized)
    (asserts! (not (has-voted tx-sender)) err-already-voted)
    (asserts! (<= (len candidate-rankings) (var-get max-candidates-per-voter)) err-invalid-candidate)
    
    (let ((vote-weight (calculate-vote-weight tx-sender)))
      ;; Record vote
      (map-set voters 
        { voter: tx-sender }
        {
          voted: true,
          candidate-id: u0, ;; Not applicable for ranked voting
          vote-block: block-height,
          vote-weight: vote-weight,
          stake-amount: (default-to u0 (get stake-amount (map-get? voter-registry { voter: tx-sender }))),
          ranked-votes: candidate-rankings,
          quadratic-credits-used: u0
        })
      
      ;; Update totals
      (var-set total-votes (+ (var-get total-votes) u1))
      (var-set total-weighted-votes (+ (var-get total-weighted-votes) vote-weight))
      (ok true))))

(define-public (vote-quadratic (votes-per-candidate (list 20 { candidate-id: uint, votes: uint })))
  (begin
    (asserts! (is-voting-period-active) err-voting-not-started)
    (asserts! (is-eq (var-get voting-type) "quadratic") err-unauthorized)
    (asserts! (not (has-voted tx-sender)) err-already-voted)
    
    (let ((total-cost (fold + (map calculate-quadratic-cost (map get-votes votes-per-candidate)) u0))
          (available-credits (default-to u0 (get stake-amount (map-get? voter-registry { voter: tx-sender })))))
      
      (asserts! (<= total-cost available-credits) err-insufficient-stake)
      
      ;; Record vote
      (map-set voters 
        { voter: tx-sender }
        {
          voted: true,
          candidate-id: u0, ;; Not applicable for quadratic voting
          vote-block: block-height,
          vote-weight: u1,
          stake-amount: available-credits,
          ranked-votes: (list),
          quadratic-credits-used: total-cost
        })
      
      ;; Update candidate counts (simplified - in practice you'd iterate through the list)
      (var-set total-votes (+ (var-get total-votes) u1))
      (ok true))))

;; Helper function for quadratic voting
(define-private (get-votes (vote-entry { candidate-id: uint, votes: uint }))
  (get votes vote-entry))

;; Election Management
(define-public (start-voting)
  (begin
    (asserts! (is-admin-or-owner) err-unauthorized)
    (asserts! (not (var-get voting-started)) err-voting-started)
    (var-set voting-started true)
    (ok true)))

(define-public (end-voting)
  (begin
    (asserts! (is-admin-or-owner) err-unauthorized)
    (asserts! (var-get voting-started) err-voting-not-started)
    
    ;; Check if quorum is met
    (let ((participation-rate (if (> (var-get total-votes) u0)
                                (/ (* (var-get total-votes) u100) (var-get total-votes))
                                u0)))
      (asserts! (>= participation-rate (var-get quorum-percentage)) err-minimum-participation)
      
      (var-set voting-started false)
      (ok true))))

;; Emergency Functions
(define-public (emergency-pause)
  (begin
    (asserts! (is-contract-owner) err-owner-only)
    (var-set voting-started false)
    (ok true)))

(define-public (emergency-reset)
  (begin
    (asserts! (is-contract-owner) err-owner-only)
    (var-set voting-started false)
    (var-set total-votes u0)
    (var-set total-weighted-votes u0)
    (ok true)))

;; Candidate removal functions (from original contract)
(define-public (remove-candidate (candidate-id uint))
  (begin
    (asserts! (is-admin-or-owner) err-unauthorized)
    (asserts! (not (var-get voting-started)) err-voting-started)
    
    (match (map-get? candidates { candidate-id: candidate-id })
      candidate-data
      (begin
        (map-delete candidate-by-name { name: (get name candidate-data) })
        (map-set candidates 
          { candidate-id: candidate-id }
          (merge candidate-data { active: false }))
        (ok true))
      err-not-found)))

(define-public (remove-candidate-by-name (name (string-ascii 50)))
  (begin
    (asserts! (is-admin-or-owner) err-unauthorized)
    (asserts! (not (var-get voting-started)) err-voting-started)
    
    (match (map-get? candidate-by-name { name: name })
      candidate-lookup
      (let ((candidate-id (get candidate-id candidate-lookup)))
        (match (map-get? candidates { candidate-id: candidate-id })
          candidate-data
          (begin
            (map-delete candidate-by-name { name: name })
            (map-set candidates 
              { candidate-id: candidate-id }
              (merge candidate-data { active: false }))
            (ok candidate-id))
          err-not-found))
      err-not-found)))