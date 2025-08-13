(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-INSUFFICIENT-BALANCE (err u201))
(define-constant ERR-INVALID-AMOUNT (err u202))
(define-constant ERR-PROJECT-NOT-VERIFIED (err u203))
(define-constant ERR-CREDITS-ALREADY-MINTED (err u204))
(define-constant ERR-INVALID-PRICE (err u205))

(define-constant CREDITS-PER-TREE u5)
(define-constant CONTRACT-OWNER tx-sender)

(define-fungible-token carbon-credits)

(define-data-var total-credits-minted uint u0)
(define-data-var total-credits-burned uint u0)

(define-map project-credits
    uint
    {
        credits-minted: uint,
        minted: bool
    }
)

(define-map credit-balances
    principal
    uint
)

(define-map marketplace-listings
    uint
    {
        seller: principal,
        amount: uint,
        price-per-credit: uint,
        active: bool
    }
)

(define-data-var next-listing-id uint u1)

(define-public (mint-credits-for-project (project-id uint))
    (let 
        ((project-data (unwrap! (contract-call? .reforestation get-project project-id) ERR-PROJECT-NOT-VERIFIED))
         (project (unwrap! project-data ERR-PROJECT-NOT-VERIFIED))
         (credits-to-mint (* (get trees-count project) CREDITS-PER-TREE))
         (existing-credits (default-to {credits-minted: u0, minted: false} (map-get? project-credits project-id))))
        
        (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
        (asserts! (get verified project) ERR-PROJECT-NOT-VERIFIED)
        (asserts! (not (get minted existing-credits)) ERR-CREDITS-ALREADY-MINTED)
        
        (try! (ft-mint? carbon-credits credits-to-mint tx-sender))
        
        (map-set project-credits project-id
            {
                credits-minted: credits-to-mint,
                minted: true
            }
        )
        
        (var-set total-credits-minted (+ (var-get total-credits-minted) credits-to-mint))
        (ok credits-to-mint)
    )
)

(define-public (transfer-credits (amount uint) (recipient principal))
    (begin
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (try! (ft-transfer? carbon-credits amount tx-sender recipient))
        (ok true)
    )
)

(define-public (burn-credits (amount uint))
    (begin
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (try! (ft-burn? carbon-credits amount tx-sender))
        (var-set total-credits-burned (+ (var-get total-credits-burned) amount))
        (ok true)
    )
)

(define-public (create-marketplace-listing (amount uint) (price-per-credit uint))
    (let ((listing-id (var-get next-listing-id)))
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> price-per-credit u0) ERR-INVALID-PRICE)
        (asserts! (>= (ft-get-balance carbon-credits tx-sender) amount) ERR-INSUFFICIENT-BALANCE)
        
        (try! (ft-transfer? carbon-credits amount tx-sender (as-contract tx-sender)))
        
        (map-set marketplace-listings listing-id
            {
                seller: tx-sender,
                amount: amount,
                price-per-credit: price-per-credit,
                active: true
            }
        )
        
        (var-set next-listing-id (+ listing-id u1))
        (ok listing-id)
    )
)

(define-public (buy-credits (listing-id uint) (amount uint))
    (let 
        ((listing (unwrap! (map-get? marketplace-listings listing-id) ERR-INVALID-AMOUNT))
         (total-cost (* amount (get price-per-credit listing))))
        
        (asserts! (get active listing) ERR-INVALID-AMOUNT)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (<= amount (get amount listing)) ERR-INSUFFICIENT-BALANCE)
        
        (try! (stx-transfer? total-cost tx-sender (get seller listing)))
        (try! (as-contract (ft-transfer? carbon-credits amount tx-sender (get seller listing))))
        
        (if (is-eq amount (get amount listing))
            (map-set marketplace-listings listing-id
                (merge listing {active: false, amount: u0})
            )
            (map-set marketplace-listings listing-id
                (merge listing {amount: (- (get amount listing) amount)})
            )
        )
        
        (ok true)
    )
)

(define-public (cancel-listing (listing-id uint))
    (let ((listing (unwrap! (map-get? marketplace-listings listing-id) ERR-INVALID-AMOUNT)))
        (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
        (asserts! (get active listing) ERR-INVALID-AMOUNT)
        
        (try! (as-contract (ft-transfer? carbon-credits (get amount listing) tx-sender (get seller listing))))
        
        (map-set marketplace-listings listing-id
            (merge listing {active: false, amount: u0})
        )
        
        (ok true)
    )
)

(define-read-only (get-credit-balance (account principal))
    (ok (ft-get-balance carbon-credits account))
)

(define-read-only (get-total-supply)
    (ok (ft-get-supply carbon-credits))
)

(define-read-only (get-project-credits (project-id uint))
    (ok (map-get? project-credits project-id))
)

(define-read-only (get-marketplace-listing (listing-id uint))
    (ok (map-get? marketplace-listings listing-id))
)

(define-read-only (get-credits-stats)
    (ok {
        total-minted: (var-get total-credits-minted),
        total-burned: (var-get total-credits-burned),
        circulating-supply: (ft-get-supply carbon-credits)
    })
)