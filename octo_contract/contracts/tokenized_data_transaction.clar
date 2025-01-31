
;; title: tokenized_data_transaction
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

;; data-token-contract
;; Implements tokenized data transactions with staking rewards and escrow

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-escrow (err u102))

;; Define data token
(define-fungible-token data-token)

;; Data structures
(define-map stakes
    { staker: principal }
    { amount: uint, start-block: uint })

(define-map escrows
    { id: uint }
    { seller: principal,
      buyer: principal,
      amount: uint,
      data-hash: (buff 32),
      status: (string-ascii 20) })

(define-data-var escrow-nonce uint u0)
(define-data-var staking-reward-rate uint u5) ;; 5% annual reward rate

;; Token management functions
(define-public (mint (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ft-mint? data-token amount recipient)))

(define-public (transfer (amount uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-owner-only)
        (ft-transfer? data-token amount sender recipient)))

;; Staking functions
(define-public (stake (amount uint))
    (let ((current-block block-height))
        (begin
            (try! (ft-transfer? data-token amount tx-sender (as-contract tx-sender)))
            (map-set stakes
                { staker: tx-sender }
                { amount: amount, start-block: current-block })
            (ok true))))

(define-public (unstake)
    (let ((stake-info (unwrap! (map-get? stakes {staker: tx-sender}) err-insufficient-balance))
          (reward (calculate-reward (get amount stake-info) (get start-block stake-info))))
        (begin
            (try! (as-contract (ft-transfer? data-token 
                (+ (get amount stake-info) reward)
                (as-contract tx-sender)
                tx-sender)))
            (map-delete stakes {staker: tx-sender})
            (ok reward))))

;; Escrow functions
(define-public (create-escrow (amount uint) (data-hash (buff 32)) (seller principal))
    (let ((escrow-id (var-get escrow-nonce)))
        (begin
            (try! (ft-transfer? data-token amount tx-sender (as-contract tx-sender)))
            (map-set escrows
                { id: escrow-id }
                { seller: seller,
                  buyer: tx-sender,
                  amount: amount,
                  data-hash: data-hash,
                  status: "pending" })
            (var-set escrow-nonce (+ escrow-id u1))
            (ok escrow-id))))

(define-public (complete-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? escrows {id: escrow-id}) err-invalid-escrow)))
        (begin
            (asserts! (is-eq (get status escrow) "pending") err-invalid-escrow)
            (asserts! (or (is-eq tx-sender (get buyer escrow))
                         (is-eq tx-sender (get seller escrow)))
                     err-owner-only)
            (try! (as-contract (ft-transfer? data-token
                (get amount escrow)
                (as-contract tx-sender)
                (get seller escrow))))
            (map-set escrows
                { id: escrow-id }
                (merge escrow { status: "completed" }))
            (ok true))))

;; Helper functions
(define-private (calculate-reward (amount uint) (start-block uint))
    (let ((blocks-staked (- block-height start-block))
          (blocks-per-year u52560))
        (* amount
           (/ (* blocks-staked (var-get staking-reward-rate))
              (* blocks-per-year u100)))))

;; Read-only functions
(define-read-only (get-stake-info (staker principal))
    (map-get? stakes {staker: staker}))

(define-read-only (get-escrow (escrow-id uint))
    (map-get? escrows {id: escrow-id}))

(define-read-only (get-balance (account principal))
    (ft-get-balance data-token account))