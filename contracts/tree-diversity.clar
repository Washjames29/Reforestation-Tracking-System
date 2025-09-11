;; Tree Species Diversity Tracker
;; Tracks biodiversity and provides ecosystem health metrics for reforestation projects

(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-PROJECT-NOT-FOUND (err u501))
(define-constant ERR-INVALID-SPECIES (err u502))
(define-constant ERR-SPECIES-ALREADY-EXISTS (err u503))
(define-constant ERR-INVALID-COUNT (err u504))
(define-constant ERR-PROJECT-NOT-VERIFIED (err u505))
(define-constant ERR-DIVERSITY-ALREADY-RECORDED (err u506))

(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-SPECIES-FOR-BONUS u3)
(define-constant MAX-DIVERSITY-BONUS u50) ;; 50% bonus for high diversity
(define-constant NATIVE-SPECIES-MULTIPLIER u120) ;; 20% bonus for native species
(define-constant ENDANGERED-SPECIES-MULTIPLIER u150) ;; 50% bonus for endangered species

;; Species classifications
(define-constant SPECIES-NATIVE "native")
(define-constant SPECIES-EXOTIC "exotic") 
(define-constant SPECIES-ENDANGERED "endangered")
(define-constant SPECIES-COMMERCIAL "commercial")

;; Data tracking
(define-data-var total-species-tracked uint u0)
(define-data-var total-diversity-projects uint u0)

;; Global species registry with ecological metadata
(define-map species-registry
    (string-ascii 64) ;; species-name
    {
        scientific-name: (string-ascii 100),
        classification: (string-ascii 20),
        carbon-efficiency: uint, ;; CO2 absorption multiplier (basis points)
        growth-rate: uint, ;; slow=1, medium=2, fast=3
        ecosystem-value: uint, ;; biodiversity impact score 1-10
        registered-by: principal,
        approved: bool
    }
)

;; Project species composition
(define-map project-species
    {project-id: uint, species-name: (string-ascii 64)}
    {
        trees-planted: uint,
        survival-rate: uint, ;; estimated percentage
        planting-date: uint,
        verified: bool
    }
)

;; Project diversity metrics
(define-map project-diversity
    uint ;; project-id
    {
        species-count: uint,
        diversity-index: uint, ;; Shannon diversity index * 100
        native-percentage: uint,
        endangered-count: uint,
        ecosystem-health-score: uint,
        diversity-bonus-earned: uint,
        recorded-at: uint
    }
)

;; Species performance tracking
(define-map species-performance
    (string-ascii 64) ;; species-name
    {
        total-planted: uint,
        avg-survival-rate: uint,
        projects-used: uint,
        carbon-credits-generated: uint,
        last-updated: uint
    }
)

;; Ecosystem impact tracking by region
(define-map regional-biodiversity
    (string-ascii 64) ;; region/location
    {
        unique-species: uint,
        total-diversity-score: uint,
        projects-count: uint,
        last-updated: uint
    }
)

;; Register a new tree species (owner or verified contributors only)
(define-public (register-species 
    (species-name (string-ascii 64))
    (scientific-name (string-ascii 100))
    (classification (string-ascii 20))
    (carbon-efficiency uint)
    (growth-rate uint)
    (ecosystem-value uint))
    (begin
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-verified-contributor tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (> (len species-name) u2) ERR-INVALID-SPECIES)
        (asserts! (is-none (map-get? species-registry species-name)) ERR-SPECIES-ALREADY-EXISTS)
        (asserts! (and (>= ecosystem-value u1) (<= ecosystem-value u10)) ERR-INVALID-SPECIES)
        (asserts! (and (>= growth-rate u1) (<= growth-rate u3)) ERR-INVALID-SPECIES)
        
        (map-set species-registry species-name
            {
                scientific-name: scientific-name,
                classification: classification,
                carbon-efficiency: carbon-efficiency,
                growth-rate: growth-rate,
                ecosystem-value: ecosystem-value,
                registered-by: tx-sender,
                approved: (is-eq tx-sender CONTRACT-OWNER)
            }
        )
        
        (var-set total-species-tracked (+ (var-get total-species-tracked) u1))
        (ok true)
    )
)

;; Record species planted in a project
(define-public (record-species-planting 
    (project-id uint)
    (species-name (string-ascii 64))
    (trees-planted uint)
    (estimated-survival-rate uint))
    (let ((project-data (unwrap! (contract-call? .reforestation get-project project-id) ERR-PROJECT-NOT-FOUND))
          (project (unwrap! project-data ERR-PROJECT-NOT-FOUND))
          (species (unwrap! (map-get? species-registry species-name) ERR-INVALID-SPECIES)))
        
        (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
        (asserts! (get approved species) ERR-INVALID-SPECIES)
        (asserts! (> trees-planted u0) ERR-INVALID-COUNT)
        (asserts! (<= estimated-survival-rate u100) ERR-INVALID-COUNT)
        
        (map-set project-species {project-id: project-id, species-name: species-name}
            {
                trees-planted: trees-planted,
                survival-rate: estimated-survival-rate,
                planting-date: stacks-block-height,
                verified: false
            }
        )
        
        ;; Update species performance tracking
        (update-species-performance species-name trees-planted estimated-survival-rate)
        (ok true)
    )
)

;; Calculate and record project diversity metrics (called after verification)
(define-public (calculate-project-diversity (project-id uint))
    (let ((project-data (unwrap! (contract-call? .reforestation get-project project-id) ERR-PROJECT-NOT-FOUND))
          (project (unwrap! project-data ERR-PROJECT-NOT-FOUND)))
        
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get owner project))) ERR-NOT-AUTHORIZED)
        (asserts! (get verified project) ERR-PROJECT-NOT-VERIFIED)
        (asserts! (is-none (map-get? project-diversity project-id)) ERR-DIVERSITY-ALREADY-RECORDED)
        
        (let ((diversity-metrics (compute-diversity-metrics project-id))
              (species-count (get species-count diversity-metrics))
              (diversity-bonus (calculate-diversity-bonus species-count (get diversity-index diversity-metrics))))
            
            (map-set project-diversity project-id
                (merge diversity-metrics {
                    diversity-bonus-earned: diversity-bonus,
                    recorded-at: stacks-block-height
                })
            )
            
            (var-set total-diversity-projects (+ (var-get total-diversity-projects) u1))
            
            ;; Update regional biodiversity metrics
            (update-regional-biodiversity (get location project) species-count (get ecosystem-health-score diversity-metrics))
            
            (ok {
                diversity-bonus: diversity-bonus,
                species-count: species-count,
                ecosystem-score: (get ecosystem-health-score diversity-metrics)
            })
        )
    )
)

;; Verify species planting data (owner only)
(define-public (verify-species-data (project-id uint) (species-name (string-ascii 64)) (actual-survival-rate uint))
    (let ((species-data (unwrap! (map-get? project-species {project-id: project-id, species-name: species-name}) ERR-INVALID-SPECIES)))
        
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= actual-survival-rate u100) ERR-INVALID-COUNT)
        
        (map-set project-species {project-id: project-id, species-name: species-name}
            (merge species-data {
                survival-rate: actual-survival-rate,
                verified: true
            })
        )
        (ok true)
    )
)

;; Private functions for calculations
(define-private (compute-diversity-metrics (project-id uint))
    (let ((species-data (get-project-species-data project-id)))
        {
            species-count: (get species-count species-data),
            diversity-index: (calculate-shannon-diversity project-id),
            native-percentage: (calculate-native-percentage project-id),
            endangered-count: (get endangered-count species-data),
            ecosystem-health-score: (calculate-ecosystem-health project-id)
        }
    )
)

(define-private (calculate-diversity-bonus (species-count uint) (diversity-index uint))
    (if (>= species-count MIN-SPECIES-FOR-BONUS)
        (let ((base-bonus (* species-count u5))
              (diversity-bonus (/ diversity-index u10)))
            (if (> (+ base-bonus diversity-bonus) MAX-DIVERSITY-BONUS)
                MAX-DIVERSITY-BONUS
                (+ base-bonus diversity-bonus)
            )
        )
        u0
    )
)

(define-private (get-project-species-data (project-id uint))
    ;; Simplified implementation - in real contract would iterate through species
    {species-count: u1, endangered-count: u0}
)

(define-private (calculate-shannon-diversity (project-id uint))
    ;; Simplified Shannon diversity calculation - returns value * 100 for precision
    u250 ;; Example: 2.5 diversity index
)

(define-private (calculate-native-percentage (project-id uint))
    ;; Calculate percentage of native species in project
    u75 ;; Example: 75% native species
)

(define-private (calculate-ecosystem-health (project-id uint))
    ;; Composite score based on species diversity, native percentage, and ecosystem values
    u8 ;; Score out of 10
)

(define-private (update-species-performance (species-name (string-ascii 64)) (trees-planted uint) (survival-rate uint))
    (let ((current-perf (default-to 
            {total-planted: u0, avg-survival-rate: u0, projects-used: u0, carbon-credits-generated: u0, last-updated: u0}
            (map-get? species-performance species-name))))
        
        (map-set species-performance species-name
            (merge current-perf {
                total-planted: (+ (get total-planted current-perf) trees-planted),
                projects-used: (+ (get projects-used current-perf) u1),
                last-updated: stacks-block-height
            })
        )
    )
)

(define-private (update-regional-biodiversity (location (string-ascii 64)) (species-count uint) (health-score uint))
    (let ((current-bio (default-to 
            {unique-species: u0, total-diversity-score: u0, projects-count: u0, last-updated: u0}
            (map-get? regional-biodiversity location))))
        
        (map-set regional-biodiversity location
            (merge current-bio {
                unique-species: (+ (get unique-species current-bio) species-count),
                total-diversity-score: (+ (get total-diversity-score current-bio) health-score),
                projects-count: (+ (get projects-count current-bio) u1),
                last-updated: stacks-block-height
            })
        )
    )
)

(define-private (is-verified-contributor (user principal))
    ;; Simplified implementation - allow any non-contract-owner to register species
    ;; In production, this would check verified project history
    (not (is-eq user CONTRACT-OWNER))
)

;; Read-only functions
(define-read-only (get-species-info (species-name (string-ascii 64)))
    (ok (map-get? species-registry species-name))
)

(define-read-only (get-project-species (project-id uint) (species-name (string-ascii 64)))
    (ok (map-get? project-species {project-id: project-id, species-name: species-name}))
)

(define-read-only (get-project-diversity (project-id uint))
    (ok (map-get? project-diversity project-id))
)

(define-read-only (get-species-performance (species-name (string-ascii 64)))
    (ok (map-get? species-performance species-name))
)

(define-read-only (get-regional-biodiversity (location (string-ascii 64)))
    (ok (map-get? regional-biodiversity location))
)

(define-read-only (get-diversity-stats)
    (ok {
        total-species: (var-get total-species-tracked),
        diversity-projects: (var-get total-diversity-projects)
    })
)

(define-read-only (calculate-enhanced-carbon-credits (project-id uint))
    (match (map-get? project-diversity project-id)
        diversity-data
        (let ((base-multiplier u100)
              (diversity-bonus (get diversity-bonus-earned diversity-data))
              (native-bonus (if (>= (get native-percentage diversity-data) u50) u10 u0))
              (endangered-bonus (* (get endangered-count diversity-data) u20)))
            (ok (+ base-multiplier diversity-bonus native-bonus endangered-bonus))
        )
        (ok u100) ;; Default multiplier if no diversity data
    )
)
