(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-INVALID-AMOUNT (err u301))
(define-constant ERR-AUCTION-NOT-FOUND (err u302))
(define-constant ERR-AUCTION-ENDED (err u303))
(define-constant ERR-AUCTION-ACTIVE (err u304))
(define-constant ERR-BID-TOO-LOW (err u305))
(define-constant ERR-INSUFFICIENT-BALANCE (err u306))
(define-constant ERR-SELF-BID (err u307))

(define-constant AUCTION-DURATION u144)
(define-constant MIN-BID-INCREMENT u5)

(define-data-var next-auction-id uint u1)

(define-map auctions
    uint
    {
        seller: principal,
        amount: uint,
        starting-price: uint,
        current-bid: uint,
        highest-bidder: (optional principal),
        end-block: uint,
        finalized: bool
    }
)

(define-map auction-bids
    {auction-id: uint, bidder: principal}
    uint
)

(define-public (create-auction (amount uint) (starting-price uint))
    (let 
        ((auction-id (var-get next-auction-id))
         (end-block (+ stacks-block-height AUCTION-DURATION)))
        
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> starting-price u0) ERR-INVALID-AMOUNT)
        (asserts! (>= (unwrap-panic (contract-call? .carbon-credits get-credit-balance tx-sender)) amount) ERR-INSUFFICIENT-BALANCE)
        
        (try! (contract-call? .carbon-credits transfer-credits amount (as-contract tx-sender)))
        
        (map-set auctions auction-id
            {
                seller: tx-sender,
                amount: amount,
                starting-price: starting-price,
                current-bid: u0,
                highest-bidder: none,
                end-block: end-block,
                finalized: false
            }
        )
        
        (var-set next-auction-id (+ auction-id u1))
        (ok auction-id)
    )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
    (let 
        ((auction (unwrap! (map-get? auctions auction-id) ERR-AUCTION-NOT-FOUND))
         (previous-bid (default-to u0 (map-get? auction-bids {auction-id: auction-id, bidder: tx-sender}))))
        
        (asserts! (not (is-eq tx-sender (get seller auction))) ERR-SELF-BID)
        (asserts! (< stacks-block-height (get end-block auction)) ERR-AUCTION-ENDED)
        (asserts! (not (get finalized auction)) ERR-AUCTION-ENDED)
        
        (let ((required-bid (if (is-eq (get current-bid auction) u0)
                                (get starting-price auction)
                                (+ (get current-bid auction) MIN-BID-INCREMENT)))
              (total-bid (+ previous-bid bid-amount)))
            
            (asserts! (>= total-bid required-bid) ERR-BID-TOO-LOW)
            
            (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
            
            (map-set auction-bids {auction-id: auction-id, bidder: tx-sender} total-bid)
            
            (let ((should-update (match (get highest-bidder auction) prev-bidder
                    (let ((prev-bid (default-to u0 (map-get? auction-bids {auction-id: auction-id, bidder: prev-bidder}))))
                        (if (> total-bid prev-bid)
                            (begin
                                (try! (as-contract (stx-transfer? prev-bid tx-sender prev-bidder)))
                                true
                            )
                            false
                        )
                    )
                    true
                )))
                (if should-update
                    (map-set auctions auction-id
                        (merge auction 
                            {
                                current-bid: total-bid,
                                highest-bidder: (some tx-sender)
                            }
                        )
                    )
                    (try! (as-contract (stx-transfer? bid-amount tx-sender tx-sender)))
                )
            )
        )
        (ok true)
    )
)

(define-public (finalize-auction (auction-id uint))
    (let ((auction (unwrap! (map-get? auctions auction-id) ERR-AUCTION-NOT-FOUND)))
        (asserts! (>= stacks-block-height (get end-block auction)) ERR-AUCTION-ACTIVE)
        (asserts! (not (get finalized auction)) ERR-AUCTION-ENDED)
        
        (match (get highest-bidder auction)
            winner 
            (begin
                (try! (as-contract (contract-call? .carbon-credits transfer-credits (get amount auction) winner)))
                (try! (as-contract (stx-transfer? (get current-bid auction) tx-sender (get seller auction))))
                (map-set auctions auction-id (merge auction {finalized: true}))
                (ok {winner: (some winner), final-price: (get current-bid auction)})
            )
            (begin
                (try! (as-contract (contract-call? .carbon-credits transfer-credits (get amount auction) (get seller auction))))
                (map-set auctions auction-id (merge auction {finalized: true}))
                (ok {winner: none, final-price: u0})
            )
        )
    )
)

(define-public (cancel-auction (auction-id uint))
    (let ((auction (unwrap! (map-get? auctions auction-id) ERR-AUCTION-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get seller auction)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get current-bid auction) u0) ERR-AUCTION-ACTIVE)
        (asserts! (not (get finalized auction)) ERR-AUCTION-ENDED)
        
        (try! (as-contract (contract-call? .carbon-credits transfer-credits (get amount auction) (get seller auction))))
        (map-set auctions auction-id (merge auction {finalized: true}))
        (ok true)
    )
)

(define-read-only (get-auction (auction-id uint))
    (ok (map-get? auctions auction-id))
)

(define-read-only (get-auction-bid (auction-id uint) (bidder principal))
    (ok (map-get? auction-bids {auction-id: auction-id, bidder: bidder}))
)

(define-read-only (get-auction-status (auction-id uint))
    (match (map-get? auctions auction-id)
        auction
        (ok {
            active: (and (< stacks-block-height (get end-block auction)) (not (get finalized auction))),
            time-remaining: (if (>= stacks-block-height (get end-block auction)) u0 (- (get end-block auction) stacks-block-height)),
            has-bids: (is-some (get highest-bidder auction))
        })
        ERR-AUCTION-NOT-FOUND
    )
)

(define-read-only (get-active-auctions)
    (ok {
        total-auctions: (- (var-get next-auction-id) u1),
        current-block: stacks-block-height
    })
)
