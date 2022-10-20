;\\\\\\\\\\\\\  _________                           \\\\\\\\\\\\\
;\\\\\\\\\\\\\ /   _____/ ____  ____   ____   ____  \\\\\\\\\\\\\
;\\\\\\\\\\\\\\_____  \_/ ___\/ __ \ /    \_/ __ \  \\\\\\\\\\\\\
;\\\\\\\\\\\\\ /        \  \__\  ___/|   |  \  ___/ \\\\\\\\\\\\\
;\\\\\\\\\\\\\/_______  /\___  >___  >___|  /\___  >\\\\\\\\\\\\\
;\\\\\\\\\\\\\        \/     \/    \/     \/     \/ \\\\\\\\\\\\\

;;;; Functions

 ;;;; Program Functions

; Generate n amount of particles as a list as vectors with mass, position and velocity
(define (generate_particles n mass_scale position_scale velocity_scale)
  (let recv ((n n) (ms mass_scale) (ps position_scale) (vs velocity_scale) (particle (make-vector 3)) (res '())) 
     (if (zero? n) res
       (begin
         (vector-set! particle 0 (* ms (random-real)))                             ;Set mass
         (vector-set! particle 1 (vector (* ps (random-real)) (* ps (random-real)))) ;Set position as a pair
         (vector-set! particle 2 (vector (* vs (random-real)) (* vs (random-real)))) ;Set velocity as a pair
         (recv (- n 1) ms ps vs (make-vector 3) (cons particle res))))))           ;Add particle to the list and reset its values. 

(define (square num)
  (* num num))

; Find the distance between two points
(define (distance p1 p2)
  (sqrt (+ (square (- (vector-ref (vector-ref p2 1) 0) (vector-ref (vector-ref p1 1) 0))) (square (- (vector-ref (vector-ref p2 1) 1) (vector-ref (vector-ref p1 1) 1))))))

; Find the force magnitude between two points with some scale
(define (force_magnitude scale p1 p2)
  (/ (* scale (vector-ref p1 0) (vector-ref p2 0)) (distance p1 p2)))

; Find the unit_vector between two points
(define (force_vector p1 p2)
  (let ((unit_vector (make-vector 2)))
    (vector-set! unit_vector 0 (- (vector-ref (vector-ref p2 1) 0) (vector-ref (vector-ref p1 1) 0)))
    (vector-set! unit_vector 1 (- (vector-ref (vector-ref p2 1) 1) (vector-ref (vector-ref p1 1) 1)))
    unit_vector))

; Returns the force between two points
(define (force scale p1 p2)
  (let ((unit_vector (force_vector p1 p2)) (force_magnitude (force_magnitude scale p1 p2)) (f (make-vector 2)))
    (vector-set! f 0 (* force_magnitude (vector-ref unit_vector 0)))
    (vector-set! f 1 (* force_magnitude (vector-ref unit_vector 1)))
    f))

; Returns the net force between the first object in an array with every other object in that array.
(define (force_between scale arr)
  (let recv ((point (car arr)) (arr (cdr arr)) (scale scale) (net_force (make-vector 2)))
       (if (null? arr) net_force
         (begin
            (let ((f (force scale point (car arr))))
                 (vector-set! net_force 0 (+ (vector-ref net_force 0) (vector-ref f 0)))  ;Update x-force
                 (vector-set! net_force 1 (+ (vector-ref net_force 1) (vector-ref f 1)))) ;Update y-force
         (recv point (cdr arr) scale net_force)))))

; returns an array but placing the first object at the back of the list
(define (reconfig_list arr)
  (begin
    (set! arr (cons (car arr) (reverse (cdr arr)))))
  (reverse arr))

; Returns a list of net forces between every point in a list and every other point in that list, serves as an initialization of the force list.
(define (internal_forces scale arr)
  (let recv ((scale scale) (arr arr) (force_list '()))
       (if (= (length arr) (length force_list)) (reverse force_list)
         (recv scale (reconfig_list arr) (cons (force_between scale arr) force_list)))))

; Updates force list for particles in one array to particles in another array
(define (external_forces scale arr1 arr2 forces)
  (let recv ((scale scale) (arr1 arr1) (arr2 arr2) (forces forces) (force (make-vector 2)) (net_forces '()))
       (if (null? arr1) (reverse net_forces)
         (begin
           (vector-set! force 0 (+ (vector-ref (car forces) 0) (vector-ref (force_between scale (cons (car arr1) arr2)) 0)))
           (vector-set! force 1 (+ (vector-ref (car forces) 1) (vector-ref (force_between scale (cons (car arr1) arr2)) 1)))
           (recv scale (cdr arr1) arr2 (cdr forces) (make-vector 2) (cons force net_forces))))))

; Accepts an array and a force list and updates the positions and velocities of the particles in that array
(define (update arr forces time)
  (let recv ((arr arr) (forces forces) (time time) (velocity (make-vector 2)) (position (make-vector 2)))
       (if (null? arr) #!void
         (begin
           (vector-set! velocity 0 (+ (vector-ref (vector-ref (car arr) 2) 0) (* time (/ (vector-ref (car forces) 0) (vector-ref (car arr) 0)))))
           (vector-set! velocity 1 (+ (vector-ref (vector-ref (car arr) 2) 1) (* time (/ (vector-ref (car forces) 1) (vector-ref (car arr) 0)))))
           (vector-set! position 0 (+ (vector-ref (vector-ref (car arr) 1) 0) (* time (vector-ref velocity 0))))
           (vector-set! position 1 (+ (vector-ref (vector-ref (car arr) 1) 1) (* time (vector-ref velocity 1))))
           (vector-set! (car arr) 1 position)
           (vector-set! (car arr) 2 velocity)
           (recv (cdr arr)  (cdr forces) time (make-vector 2) (make-vector 2))))))

 ;;;; CSV write

(define-macro (write_csv output_port . body)
  `(let recv ((line (append ,@body)) (output_port ,output_port))
        (if (= (length line) 1) 
            (begin
              (write (vector-ref (car line) 1) output_port)
              (write-char #\newline output_port))
          (begin
            (write (vector-ref (car line) 1) output_port)
            (write-char #\, output_port)
            (recv (cdr line) output_port)))))

