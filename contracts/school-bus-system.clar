;; School Bus Route Optimization System
;; A comprehensive student transportation system with route planning, safety monitoring, and parent notifications

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-STUDENT-NOT-FOUND (err u101))
(define-constant ERR-ROUTE-NOT-FOUND (err u102))
(define-constant ERR-BUS-NOT-FOUND (err u103))
(define-constant ERR-INVALID-STATUS (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-INVALID-COORDINATES (err u106))

;; Contract deployer as admin
(define-constant CONTRACT-ADMIN tx-sender)

;; Data structures
(define-map students
  { student-id: uint }
  { 
    name: (string-ascii 50),
    grade: uint,
    home-address: (string-ascii 100),
    home-coordinates: { lat: int, lng: int },
    parent-contact: (string-ascii 50),
    assigned-route: (optional uint),
    pickup-time: (optional uint),
    status: (string-ascii 20)
  }
)

(define-map routes
  { route-id: uint }
  {
    name: (string-ascii 50),
    bus-id: uint,
    driver-id: uint,
    school-coordinates: { lat: int, lng: int },
    max-capacity: uint,
    current-occupancy: uint,
    estimated-duration: uint,
    status: (string-ascii 20)
  }
)

(define-map buses
  { bus-id: uint }
  {
    license-plate: (string-ascii 20),
    capacity: uint,
    gps-enabled: bool,
    current-location: { lat: int, lng: int },
    fuel-level: uint,
    maintenance-status: (string-ascii 20),
    safety-features: (list 10 (string-ascii 30))
  }
)

(define-map drivers
  { driver-id: uint }
  {
    name: (string-ascii 50),
    license-number: (string-ascii 30),
    contact: (string-ascii 50),
    experience-years: uint,
    safety-rating: uint,
    assigned-bus: (optional uint)
  }
)

(define-map route-stops
  { route-id: uint, stop-order: uint }
  {
    stop-name: (string-ascii 50),
    coordinates: { lat: int, lng: int },
    estimated-arrival: uint,
    students-at-stop: (list 20 uint)
  }
)

(define-map emergency-contacts
  { student-id: uint }
  {
    primary-contact: (string-ascii 50),
    secondary-contact: (string-ascii 50),
    medical-info: (string-ascii 100),
    special-needs: (string-ascii 100)
  }
)

;; Counters
(define-data-var next-student-id uint u1)
(define-data-var next-route-id uint u1)
(define-data-var next-bus-id uint u1)
(define-data-var next-driver-id uint u1)

;; System status
(define-data-var system-active bool true)

;; Private functions
(define-private (is-admin (caller principal))
  (is-eq caller CONTRACT-ADMIN)
)

(define-private (calculate-distance (coord1 { lat: int, lng: int }) (coord2 { lat: int, lng: int }))
  ;; Simplified distance calculation (Manhattan distance scaled)
  (+ (if (> (get lat coord1) (get lat coord2))
       (- (get lat coord1) (get lat coord2))
       (- (get lat coord2) (get lat coord1)))
     (if (> (get lng coord1) (get lng coord2))
       (- (get lng coord1) (get lng coord2))
       (- (get lng coord2) (get lng coord1))))
)

(define-private (is-valid-coordinates (coords { lat: int, lng: int }))
  (and 
    (>= (get lat coords) -900000) ;; -90.0000 degrees
    (<= (get lat coords) 900000)  ;; 90.0000 degrees
    (>= (get lng coords) -1800000) ;; -180.0000 degrees
    (<= (get lng coords) 1800000)) ;; 180.0000 degrees
)

;; Public functions

;; Student management
(define-public (register-student 
  (name (string-ascii 50))
  (grade uint)
  (home-address (string-ascii 100))
  (home-coordinates { lat: int, lng: int })
  (parent-contact (string-ascii 50)))
  (let ((student-id (var-get next-student-id)))
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-valid-coordinates home-coordinates) ERR-INVALID-COORDINATES)
    (map-set students 
      { student-id: student-id }
      {
        name: name,
        grade: grade,
        home-address: home-address,
        home-coordinates: home-coordinates,
        parent-contact: parent-contact,
        assigned-route: none,
        pickup-time: none,
        status: "registered"
      }
    )
    (var-set next-student-id (+ student-id u1))
    (ok student-id)
  )
)

(define-public (assign-student-to-route (student-id uint) (route-id uint) (pickup-time uint))
  (let ((student-data (unwrap! (map-get? students { student-id: student-id }) ERR-STUDENT-NOT-FOUND))
        (route-data (unwrap! (map-get? routes { route-id: route-id }) ERR-ROUTE-NOT-FOUND)))
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (< (get current-occupancy route-data) (get max-capacity route-data)) ERR-INVALID-STATUS)
    
    ;; Update student assignment
    (map-set students
      { student-id: student-id }
      (merge student-data { 
        assigned-route: (some route-id),
        pickup-time: (some pickup-time),
        status: "assigned"
      })
    )
    
    ;; Update route occupancy
    (map-set routes
      { route-id: route-id }
      (merge route-data { current-occupancy: (+ (get current-occupancy route-data) u1) })
    )
    (ok true)
  )
)

;; Route management
(define-public (create-route 
  (name (string-ascii 50))
  (bus-id uint)
  (driver-id uint)
  (school-coordinates { lat: int, lng: int })
  (max-capacity uint)
  (estimated-duration uint))
  (let ((route-id (var-get next-route-id)))
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-valid-coordinates school-coordinates) ERR-INVALID-COORDINATES)
    (asserts! (is-some (map-get? buses { bus-id: bus-id })) ERR-BUS-NOT-FOUND)
    
    (map-set routes
      { route-id: route-id }
      {
        name: name,
        bus-id: bus-id,
        driver-id: driver-id,
        school-coordinates: school-coordinates,
        max-capacity: max-capacity,
        current-occupancy: u0,
        estimated-duration: estimated-duration,
        status: "active"
      }
    )
    (var-set next-route-id (+ route-id u1))
    (ok route-id)
  )
)

(define-public (add-route-stop 
  (route-id uint)
  (stop-order uint)
  (stop-name (string-ascii 50))
  (coordinates { lat: int, lng: int })
  (estimated-arrival uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? routes { route-id: route-id })) ERR-ROUTE-NOT-FOUND)
    (asserts! (is-valid-coordinates coordinates) ERR-INVALID-COORDINATES)
    
    (map-set route-stops
      { route-id: route-id, stop-order: stop-order }
      {
        stop-name: stop-name,
        coordinates: coordinates,
        estimated-arrival: estimated-arrival,
        students-at-stop: (list)
      }
    )
    (ok true)
  )
)

;; Bus management
(define-public (register-bus 
  (license-plate (string-ascii 20))
  (capacity uint)
  (gps-enabled bool)
  (fuel-level uint)
  (safety-features (list 10 (string-ascii 30))))
  (let ((bus-id (var-get next-bus-id)))
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set buses
      { bus-id: bus-id }
      {
        license-plate: license-plate,
        capacity: capacity,
        gps-enabled: gps-enabled,
        current-location: { lat: 0, lng: 0 },
        fuel-level: fuel-level,
        maintenance-status: "operational",
        safety-features: safety-features
      }
    )
    (var-set next-bus-id (+ bus-id u1))
    (ok bus-id)
  )
)

(define-public (update-bus-location (bus-id uint) (coordinates { lat: int, lng: int }))
  (let ((bus-data (unwrap! (map-get? buses { bus-id: bus-id }) ERR-BUS-NOT-FOUND)))
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-valid-coordinates coordinates) ERR-INVALID-COORDINATES)
    
    (map-set buses
      { bus-id: bus-id }
      (merge bus-data { current-location: coordinates })
    )
    (ok true)
  )
)

;; Driver management
(define-public (register-driver 
  (name (string-ascii 50))
  (license-number (string-ascii 30))
  (contact (string-ascii 50))
  (experience-years uint)
  (safety-rating uint))
  (let ((driver-id (var-get next-driver-id)))
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set drivers
      { driver-id: driver-id }
      {
        name: name,
        license-number: license-number,
        contact: contact,
        experience-years: experience-years,
        safety-rating: safety-rating,
        assigned-bus: none
      }
    )
    (var-set next-driver-id (+ driver-id u1))
    (ok driver-id)
  )
)

;; Emergency and safety functions
(define-public (set-emergency-contact 
  (student-id uint)
  (primary-contact (string-ascii 50))
  (secondary-contact (string-ascii 50))
  (medical-info (string-ascii 100))
  (special-needs (string-ascii 100)))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? students { student-id: student-id })) ERR-STUDENT-NOT-FOUND)
    
    (map-set emergency-contacts
      { student-id: student-id }
      {
        primary-contact: primary-contact,
        secondary-contact: secondary-contact,
        medical-info: medical-info,
        special-needs: special-needs
      }
    )
    (ok true)
  )
)

(define-public (update-student-status (student-id uint) (new-status (string-ascii 20)))
  (let ((student-data (unwrap! (map-get? students { student-id: student-id }) ERR-STUDENT-NOT-FOUND)))
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set students
      { student-id: student-id }
      (merge student-data { status: new-status })
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-student (student-id uint))
  (map-get? students { student-id: student-id })
)

(define-read-only (get-route (route-id uint))
  (map-get? routes { route-id: route-id })
)

(define-read-only (get-bus (bus-id uint))
  (map-get? buses { bus-id: bus-id })
)

(define-read-only (get-driver (driver-id uint))
  (map-get? drivers { driver-id: driver-id })
)

(define-read-only (get-route-stop (route-id uint) (stop-order uint))
  (map-get? route-stops { route-id: route-id, stop-order: stop-order })
)

(define-read-only (get-emergency-contact (student-id uint))
  (map-get? emergency-contacts { student-id: student-id })
)

(define-read-only (calculate-route-efficiency (route-id uint))
  (match (map-get? routes { route-id: route-id })
    route-data 
      (let ((occupancy-rate (/ (* (get current-occupancy route-data) u100) (get max-capacity route-data))))
        (ok {
          occupancy-percentage: occupancy-rate,
          estimated-duration: (get estimated-duration route-data),
          efficiency-score: (if (> occupancy-rate u75) u5
                           (if (> occupancy-rate u50) u4
                           (if (> occupancy-rate u25) u3
                           (if (> occupancy-rate u10) u2 u1))))
        })
      )
    ERR-ROUTE-NOT-FOUND
  )
)
