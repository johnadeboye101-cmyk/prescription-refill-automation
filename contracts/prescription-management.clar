
;; title: prescription-management
;; version: 1.0.0
;; summary: Prescription refill automation platform for chronic care patients
;; description: Smart contract for managing prescription refills, pharmacy coordination, and insurance processing

;; constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_REFILL_DATE (err u103))
(define-constant ERR_INSUFFICIENT_REFILLS (err u104))
(define-constant ERR_REFILL_TOO_EARLY (err u105))
(define-constant ERR_INACTIVE_PRESCRIPTION (err u106))

;; data vars
(define-data-var next-prescription-id uint u1)
(define-data-var contract-owner principal tx-sender)

;; data maps
(define-map prescriptions uint 
  {
    patient: principal,
    medication: (string-ascii 100),
    dosage: (string-ascii 50),
    quantity: uint,
    refills-remaining: uint,
    days-supply: uint,
    prescribing-doctor: (string-ascii 100),
    pharmacy: (string-ascii 100),
    insurance-id: (string-ascii 50),
    issue-date: uint,
    last-refill-date: uint,
    active: bool
  }
)

(define-map refill-history uint
  {
    prescription-id: uint,
    refill-date: uint,
    pharmacy: (string-ascii 100),
    insurance-covered: bool,
    copay-amount: uint,
    processed-by: principal
  }
)

(define-map patient-reminders principal
  {
    next-refill-date: uint,
    reminder-frequency: uint,
    last-reminder-sent: uint
  }
)

(define-map authorized-pharmacies principal bool)
(define-map authorized-doctors principal bool)

;; public functions

;; Initialize prescription
(define-public (create-prescription 
    (patient principal)
    (medication (string-ascii 100))
    (dosage (string-ascii 50))
    (quantity uint)
    (total-refills uint)
    (days-supply uint)
    (prescribing-doctor (string-ascii 100))
    (pharmacy (string-ascii 100))
    (insurance-id (string-ascii 50))
  )
  (let 
    (
      (prescription-id (var-get next-prescription-id))
      (current-block-height stacks-block-height)
    )
    (asserts! (is-authorized-doctor tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> total-refills u0) (err u107))
    (asserts! (> days-supply u0) (err u108))
    
    (map-set prescriptions prescription-id
      {
        patient: patient,
        medication: medication,
        dosage: dosage,
        quantity: quantity,
        refills-remaining: total-refills,
        days-supply: days-supply,
        prescribing-doctor: prescribing-doctor,
        pharmacy: pharmacy,
        insurance-id: insurance-id,
        issue-date: current-block-height,
        last-refill-date: u0,
        active: true
      }
    )
    
    ;; Set up automatic refill reminders
    (map-set patient-reminders patient
      {
        next-refill-date: (+ current-block-height days-supply),
        reminder-frequency: u7, ;; 7 blocks (days) before refill needed
        last-reminder-sent: u0
      }
    )
    
    (var-set next-prescription-id (+ prescription-id u1))
    (ok prescription-id)
  )
)

;; Process prescription refill
(define-public (process-refill 
    (prescription-id uint)
    (pharmacy-principal principal)
    (insurance-covered bool)
    (copay-amount uint)
  )
  (let 
    (
      (prescription-data (unwrap! (map-get? prescriptions prescription-id) ERR_NOT_FOUND))
      (current-block-height stacks-block-height)
      (last-refill (get last-refill-date prescription-data))
      (days-supply (get days-supply prescription-data))
    )
    (asserts! (is-authorized-pharmacy pharmacy-principal) ERR_UNAUTHORIZED)
    (asserts! (get active prescription-data) ERR_INACTIVE_PRESCRIPTION)
    (asserts! (> (get refills-remaining prescription-data) u0) ERR_INSUFFICIENT_REFILLS)
    
    ;; Check if enough time has passed since last refill (75% of days supply)
    (asserts! (or (is-eq last-refill u0) 
                  (>= current-block-height (+ last-refill (/ (* days-supply u3) u4))))
              ERR_REFILL_TOO_EARLY)
    
    ;; Update prescription
    (map-set prescriptions prescription-id
      (merge prescription-data
        {
          refills-remaining: (- (get refills-remaining prescription-data) u1),
          last-refill-date: current-block-height
        }
      )
    )
    
    ;; Record refill history
    (map-set refill-history (var-get next-prescription-id)
      {
        prescription-id: prescription-id,
        refill-date: current-block-height,
        pharmacy: (get pharmacy prescription-data),
        insurance-covered: insurance-covered,
        copay-amount: copay-amount,
        processed-by: pharmacy-principal
      }
    )
    
    ;; Update reminder schedule
    (map-set patient-reminders (get patient prescription-data)
      {
        next-refill-date: (+ current-block-height days-supply),
        reminder-frequency: u7,
        last-reminder-sent: u0
      }
    )
    
    (ok true)
  )
)

;; Deactivate prescription
(define-public (deactivate-prescription (prescription-id uint))
  (let 
    (
      (prescription-data (unwrap! (map-get? prescriptions prescription-id) ERR_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get patient prescription-data))
                  (is-authorized-doctor tx-sender)) ERR_UNAUTHORIZED)
    
    (map-set prescriptions prescription-id
      (merge prescription-data { active: false })
    )
    (ok true)
  )
)

;; Authorize pharmacy
(define-public (authorize-pharmacy (pharmacy-principal principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set authorized-pharmacies pharmacy-principal true)
    (ok true)
  )
)

;; Authorize doctor
(define-public (authorize-doctor (doctor-principal principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set authorized-doctors doctor-principal true)
    (ok true)
  )
)

;; read only functions

;; Get prescription details
(define-read-only (get-prescription (prescription-id uint))
  (map-get? prescriptions prescription-id)
)

;; Check if refill is needed
(define-read-only (is-refill-needed (prescription-id uint))
  (match (map-get? prescriptions prescription-id)
    prescription-data 
      (let 
        (
          (last-refill (get last-refill-date prescription-data))
          (days-supply (get days-supply prescription-data))
          (current-block stacks-block-height)
        )
        (and 
          (get active prescription-data)
          (> (get refills-remaining prescription-data) u0)
          (or (is-eq last-refill u0)
              (>= current-block (+ last-refill (/ (* days-supply u3) u4))))
        )
      )
    false
  )
)

;; Get patient reminder info
(define-read-only (get-patient-reminders (patient principal))
  (map-get? patient-reminders patient)
)

;; Check if pharmacy is authorized
(define-read-only (is-authorized-pharmacy (pharmacy-principal principal))
  (default-to false (map-get? authorized-pharmacies pharmacy-principal))
)

;; Check if doctor is authorized
(define-read-only (is-authorized-doctor (doctor-principal principal))
  (default-to false (map-get? authorized-doctors doctor-principal))
)

;; Get refill history
(define-read-only (get-refill-history (refill-id uint))
  (map-get? refill-history refill-id)
)

;; Calculate days until next refill
(define-read-only (days-until-refill (prescription-id uint))
  (match (map-get? prescriptions prescription-id)
    prescription-data
      (let 
        (
          (last-refill (get last-refill-date prescription-data))
          (days-supply (get days-supply prescription-data))
          (current-block stacks-block-height)
          (next-refill-block (+ last-refill days-supply))
        )
        (if (> next-refill-block current-block)
            (some (- next-refill-block current-block))
            (some u0)
        )
      )
    none
  )
)
