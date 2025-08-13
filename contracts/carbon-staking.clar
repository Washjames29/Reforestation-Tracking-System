;; Carbon Credit Staking & Yield System
;; Enables staking of carbon credits for yield generation

(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-INVALID-AMOUNT (err u401))
(define-constant ERR-INSUFFICIENT-BALANCE (err u402))
(define-constant ERR-POOL-NOT-FOUND (err u403))
(define-constant ERR-STAKE-NOT-FOUND (err u404))
(define-constant ERR-STAKE-LOCKED (err u405))
(define-constant ERR-POOL-INACTIVE (err u406))
(define-constant ERR-INVALID-POOL-CONFIG (err u407))

;; Pool configurations with different lock periods and APY rates
(define-constant POOL-SHORT-TERM u1)    ;; 30 days, 5% APY
(define-constant POOL-MEDIUM-TERM u2)   ;; 90 days, 12% APY  
(define-constant POOL-LONG-TERM u3)     ;; 180 days, 25% APY

(define-constant SHORT-TERM-BLOCKS u4320)   ;; ~30 days
(define-constant MEDIUM-TERM-BLOCKS u12960) ;; ~90 days
(define-constant LONG-TERM-BLOCKS u25920)   ;; ~180 days

(define-constant SHORT-TERM-APY u500)    ;; 5% in basis points
(define-constant MEDIUM-TERM-APY u1200)  ;; 12% in basis points
(define-constant LONG-TERM-APY u2500)    ;; 25% in basis points

(define-constant BLOCKS-PER-YEAR u52560) ;; Approximate blocks per year
(define-constant BASIS-POINTS u10000)    ;; For percentage calculations
(define-constant EARLY-WITHDRAWAL-PENALTY u2000) ;; 20% penalty

(define-data-var total-staked uint u0)
(define-data-var next-stake-id uint u1)
(define-data-var contract-treasury uint u0)

;; Pool definitions with lock periods and reward rates
(define-map staking-pools
    uint
    {
        name: (string-ascii 32),
        lock-blocks: uint,
        apy-basis-points: uint,
        total-staked: uint,
        active: bool,
        max-capacity: uint
    }
)

;; Individual stake records
(define-map user-stakes
    uint
    {
        staker: principal,
        pool-id: uint,
        amount: uint,
        start-block: uint,
        last-reward-block: uint,
        accumulated-rewards: uint,
        withdrawn: bool
    }
)

;; User stake tracking for easy lookup
(define-map user-stake-count
    principal
    uint
)

;; Pool reward distribution tracking
(define-map pool-reward-history
    {pool-id: uint, block-height: uint}
    uint
)

;; Initialize staking pools
(map-set staking-pools POOL-SHORT-TERM
    {
        name: "Carbon Short Stake",
        lock-blocks: SHORT-TERM-BLOCKS,
        apy-basis-points: SHORT-TERM-APY,
        total-staked: u0,
        active: true,
        max-capacity: u1000000 ;; 1M carbon credits max
    }
)

(map-set staking-pools POOL-MEDIUM-TERM
    {
        name: "Carbon Medium Stake", 
        lock-blocks: MEDIUM-TERM-BLOCKS,
        apy-basis-points: MEDIUM-TERM-APY,
        total-staked: u0,
        active: true,
        max-capacity: u500000 ;; 500K carbon credits max
    }
)

(map-set staking-pools POOL-LONG-TERM
    {
        name: "Carbon Long Stake",
        lock-blocks: LONG-TERM-BLOCKS,
        apy-basis-points: LONG-TERM-APY,
        total-staked: u0,
        active: true,
        max-capacity: u250000 ;; 250K carbon credits max
    }
)

;; Stake carbon credits in specified pool
(define-public (stake-credits (pool-id uint) (amount uint))
    (let 
        ((pool (unwrap! (map-get? staking-pools pool-id) ERR-POOL-NOT-FOUND))
         (stake-id (var-get next-stake-id))
         (user-count (default-to u0 (map-get? user-stake-count tx-sender))))
        
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (get active pool) ERR-POOL-INACTIVE)
        (asserts! (<= (+ (get total-staked pool) amount) (get max-capacity pool)) ERR-INVALID-AMOUNT)
        (asserts! (>= (unwrap-panic (contract-call? .carbon-credits get-credit-balance tx-sender)) amount) ERR-INSUFFICIENT-BALANCE)
        
        ;; Transfer credits to staking contract
        (try! (contract-call? .carbon-credits transfer-credits amount (as-contract tx-sender)))
        
        ;; Create stake record
        (map-set user-stakes stake-id
            {
                staker: tx-sender,
                pool-id: pool-id,
                amount: amount,
                start-block: stacks-block-height,
                last-reward-block: stacks-block-height,
                accumulated-rewards: u0,
                withdrawn: false
            }
        )
        
        ;; Update pool stats
        (map-set staking-pools pool-id
            (merge pool {total-staked: (+ (get total-staked pool) amount)})
        )
        
        ;; Update global stats
        (var-set total-staked (+ (var-get total-staked) amount))
        (var-set next-stake-id (+ stake-id u1))
        (map-set user-stake-count tx-sender (+ user-count u1))
        
        (ok stake-id)
    )
)

;; Calculate pending rewards for a stake
(define-read-only (calculate-pending-rewards (stake-id uint))
    (match (map-get? user-stakes stake-id)
        stake
        (match (map-get? staking-pools (get pool-id stake))
            pool
            (let 
                ((blocks-elapsed (- stacks-block-height (get last-reward-block stake)))
                 (apy-per-block (/ (get apy-basis-points pool) BLOCKS-PER-YEAR))
                 (reward-amount (/ (* (* (get amount stake) apy-per-block) blocks-elapsed) BASIS-POINTS)))
                (ok reward-amount)
            )
            ERR-POOL-NOT-FOUND
        )
        ERR-STAKE-NOT-FOUND
    )
)

;; Claim accumulated rewards without unstaking
(define-public (claim-rewards (stake-id uint))
    (let 
        ((stake (unwrap! (map-get? user-stakes stake-id) ERR-STAKE-NOT-FOUND))
         (pending-rewards (unwrap! (calculate-pending-rewards stake-id) ERR-STAKE-NOT-FOUND)))
        
        (asserts! (is-eq tx-sender (get staker stake)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get withdrawn stake)) ERR-STAKE-NOT-FOUND)
        
        (if (> pending-rewards u0)
            (begin
                ;; Transfer reward tokens from staking contract to user
                (try! (as-contract (contract-call? .carbon-credits transfer-credits pending-rewards (get staker stake))))
                
                ;; Update stake record with claimed rewards
                (map-set user-stakes stake-id
                    (merge stake 
                        {
                            last-reward-block: stacks-block-height,
                            accumulated-rewards: (+ (get accumulated-rewards stake) pending-rewards)
                        }
                    )
                )
                (ok pending-rewards)
            )
            (ok u0)
        )
    )
)

;; Unstake credits after lock period expires
(define-public (unstake-credits (stake-id uint))
    (let 
        ((stake (unwrap! (map-get? user-stakes stake-id) ERR-STAKE-NOT-FOUND))
         (pool (unwrap! (map-get? staking-pools (get pool-id stake)) ERR-POOL-NOT-FOUND))
         (lock-end-block (+ (get start-block stake) (get lock-blocks pool)))
         (pending-rewards (unwrap! (calculate-pending-rewards stake-id) ERR-STAKE-NOT-FOUND)))
        
        (asserts! (is-eq tx-sender (get staker stake)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get withdrawn stake)) ERR-STAKE-NOT-FOUND)
        (asserts! (>= stacks-block-height lock-end-block) ERR-STAKE-LOCKED)
        
        ;; Claim any pending rewards first
        (if (> pending-rewards u0)
            (try! (as-contract (contract-call? .carbon-credits transfer-credits pending-rewards (get staker stake))))
            true
        )
        
        ;; Return staked credits
        (try! (as-contract (contract-call? .carbon-credits transfer-credits (get amount stake) (get staker stake))))
        
        ;; Mark stake as withdrawn
        (map-set user-stakes stake-id
            (merge stake 
                {
                    withdrawn: true,
                    last-reward-block: stacks-block-height,
                    accumulated-rewards: (+ (get accumulated-rewards stake) pending-rewards)
                }
            )
        )
        
        ;; Update pool stats
        (map-set staking-pools (get pool-id stake)
            (merge pool {total-staked: (- (get total-staked pool) (get amount stake))})
        )
        
        ;; Update global stats
        (var-set total-staked (- (var-get total-staked) (get amount stake)))
        
        (ok {
            principal-returned: (get amount stake),
            rewards-claimed: pending-rewards,
            total-earned: (+ (get accumulated-rewards stake) pending-rewards)
        })
    )
)

;; Emergency withdrawal with penalty (before lock expires)
(define-public (emergency-withdraw (stake-id uint))
    (let 
        ((stake (unwrap! (map-get? user-stakes stake-id) ERR-STAKE-NOT-FOUND))
         (pool (unwrap! (map-get? staking-pools (get pool-id stake)) ERR-POOL-NOT-FOUND))
         (penalty-amount (/ (* (get amount stake) EARLY-WITHDRAWAL-PENALTY) BASIS-POINTS))
         (return-amount (- (get amount stake) penalty-amount)))
        
        (asserts! (is-eq tx-sender (get staker stake)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get withdrawn stake)) ERR-STAKE-NOT-FOUND)
        
        ;; Transfer reduced amount back to user
        (try! (as-contract (contract-call? .carbon-credits transfer-credits return-amount (get staker stake))))
        
        ;; Penalty goes to treasury
        (var-set contract-treasury (+ (var-get contract-treasury) penalty-amount))
        
        ;; Mark stake as withdrawn
        (map-set user-stakes stake-id
            (merge stake {withdrawn: true})
        )
        
        ;; Update pool stats
        (map-set staking-pools (get pool-id stake)
            (merge pool {total-staked: (- (get total-staked pool) (get amount stake))})
        )
        
        ;; Update global stats
        (var-set total-staked (- (var-get total-staked) (get amount stake)))
        
        (ok {
            amount-returned: return-amount,
            penalty-paid: penalty-amount
        })
    )
)

;; Read-only functions for getting staking information
(define-read-only (get-stake-info (stake-id uint))
    (ok (map-get? user-stakes stake-id))
)

(define-read-only (get-pool-info (pool-id uint))
    (ok (map-get? staking-pools pool-id))
)

(define-read-only (get-user-stake-count (user principal))
    (ok (default-to u0 (map-get? user-stake-count user)))
)

(define-read-only (get-staking-stats)
    (ok {
        total-staked: (var-get total-staked),
        total-stakes: (- (var-get next-stake-id) u1),
        contract-treasury: (var-get contract-treasury)
    })
)

;; Calculate APY for display purposes
(define-read-only (get-pool-apy (pool-id uint))
    (match (map-get? staking-pools pool-id)
        pool (ok (get apy-basis-points pool))
        ERR-POOL-NOT-FOUND
    )
)

