(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INVALID-LOCATION (err u102))
(define-constant ERR-PROJECT-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-VERIFIED (err u104))

(define-constant REWARD-PER-TREE u10)
(define-constant CONTRACT-OWNER tx-sender)

(define-data-var total-trees-planted uint u0)
(define-data-var total-projects uint u0)

(define-map projects 
    uint 
    {
        owner: principal,
        location: (string-ascii 64),
        trees-count: uint,
        verified: bool,
        reward-claimed: bool,
        created-at: uint
    }
)

(define-map community-stats
    principal
    {
        total-trees: uint,
        total-rewards: uint,
        projects-count: uint
    }
)

(define-public (register-project (location (string-ascii 64)) (trees-count uint))
    (let
        (
            (project-id (+ (var-get total-projects) u1))
            (stats (default-to 
                {total-trees: u0, total-rewards: u0, projects-count: u0} 
                (map-get? community-stats tx-sender)))
        )
        (asserts! (> trees-count u0) ERR-INVALID-AMOUNT)
        (map-set projects project-id
            {
                owner: tx-sender,
                location: location,
                trees-count: trees-count,
                verified: false,
                reward-claimed: false,
                created-at: stacks-block-height
            }
        )
        (map-set community-stats tx-sender
            {
                total-trees: (+ (get total-trees stats) trees-count),
                total-rewards: (get total-rewards stats),
                projects-count: (+ (get projects-count stats) u1)
            }
        )
        (var-set total-projects project-id)
        (ok project-id)
    )
)

(define-public (verify-project (project-id uint))
    (let ((project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND)))
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (not (get verified project)) ERR-ALREADY-VERIFIED)
        (map-set projects project-id
            (merge project {verified: true})
        )
        (var-set total-trees-planted (+ (var-get total-trees-planted) (get trees-count project)))
        (ok true)
    )
)

(define-public (claim-reward (project-id uint))
    (let 
        (
            (project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
            (stats (default-to 
                {total-trees: u0, total-rewards: u0, projects-count: u0} 
                (map-get? community-stats (get owner project))))
            (reward-amount (* (get trees-count project) REWARD-PER-TREE))
        )
        (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
        (asserts! (get verified project) ERR-NOT-AUTHORIZED)
        (asserts! (not (get reward-claimed project)) ERR-ALREADY-VERIFIED)
        (map-set projects project-id
            (merge project {reward-claimed: true})
        )
        (map-set community-stats (get owner project)
            (merge stats 
                {total-rewards: (+ (get total-rewards stats) reward-amount)}
            )
        )
        (ok reward-amount)
    )
)

(define-read-only (get-project (project-id uint))
    (ok (map-get? projects project-id))
)

(define-read-only (get-community-stats (community principal))
    (ok (map-get? community-stats community))
)

(define-read-only (get-total-trees)
    (ok (var-get total-trees-planted))
)

(define-read-only (get-total-projects)
    (ok (var-get total-projects))
)



(define-constant ERR-NO-CHANGES (err u105))

(define-public (update-project (project-id uint) (new-location (string-ascii 64)) (additional-trees uint))
    (let 
        ((project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
         (stats (default-to 
            {total-trees: u0, total-rewards: u0, projects-count: u0} 
            (map-get? community-stats tx-sender))))
        (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get verified project)) ERR-NOT-AUTHORIZED)
        (asserts! (or (> additional-trees u0) (not (is-eq new-location (get location project)))) ERR-NO-CHANGES)
        
        (map-set projects project-id
            (merge project 
                {
                    location: new-location,
                    trees-count: (+ (get trees-count project) additional-trees)
                }
            )
        )
        (map-set community-stats tx-sender
            (merge stats 
                {total-trees: (+ (get total-trees stats) additional-trees)}
            )
        )
        (ok true)
    )
)


(define-constant ERR-TRANSFER-FAILED (err u106))

(define-public (transfer-project (project-id uint) (new-owner principal))
    (let 
        ((project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
         (old-stats (default-to 
            {total-trees: u0, total-rewards: u0, projects-count: u0} 
            (map-get? community-stats tx-sender)))
         (new-stats (default-to 
            {total-trees: u0, total-rewards: u0, projects-count: u0} 
            (map-get? community-stats new-owner))))
        
        (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get verified project)) ERR-NOT-AUTHORIZED)
        
        (map-set projects project-id
            (merge project {owner: new-owner})
        )
        (map-set community-stats tx-sender
            (merge old-stats 
                {
                    total-trees: (- (get total-trees old-stats) (get trees-count project)),
                    projects-count: (- (get projects-count old-stats) u1)
                }
            )
        )
        (map-set community-stats new-owner
            (merge new-stats 
                {
                    total-trees: (+ (get total-trees new-stats) (get trees-count project)),
                    projects-count: (+ (get projects-count new-stats) u1)
                }
            )
        )
        (ok true)
    )
)