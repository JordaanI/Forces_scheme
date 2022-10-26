;\\\\\\\\\\\\\  _________                           \\\\\\\\\\\\\
;\\\\\\\\\\\\\ /   _____/ ____  ____   ____   ____  \\\\\\\\\\\\\
;\\\\\\\\\\\\\\_____  \_/ ___\/ __ \ /    \_/ __ \  \\\\\\\\\\\\\
;\\\\\\\\\\\\\ /        \  \__\  ___/|   |  \  ___/ \\\\\\\\\\\\\
;\\\\\\\\\\\\\/_______  /\___  >___  >___|  /\___  >\\\\\\\\\\\\\
;\\\\\\\\\\\\\        \/     \/    \/     \/     \/ \\\\\\\\\\\\\

(include "functions.scm")
(define o (open-output-file "data.csv"))

; Runtime in seconds
(define run_time 30)
(define t 0.01)
(define amount (/ run_time t))

; Initialize particle arrays
(define 10_mass_array (generate_particles 100 10 500 500))
(define 5_mass_array (generate_particles 100 5 500 500))
(define 1_mass_array (generate_particles 100 1 500 500))

; Initialize scalers
(define 10ma_force_scale 2.63)
(define 5ma_force_scale 3)
(define 1ma_force_scale 3.34)


; Initialize force arrays
(define forces_10ma '())
(define forces_5ma '())
(define forces_1ma '())

(do ((i 0 (+ i 1)))
    ((= amount i) #!void)
  (begin
    
    ; Update forces between particles
    
    ; Update internal forces
    (set! forces_10ma (internal_forces 10ma_force_scale 10_mass_array))
    (set! forces_5ma (internal_forces 5ma_force_scale 5_mass_array))
    (set! forces_1ma (internal_forces 1ma_force_scale 1_mass_array))
    
    ; Update external forces
    
    ; Update 10ma
    (set! forces_10ma (external_forces 10ma_force_scale 10_mass_array 5_mass_array forces_10ma))
    (set! forces_10ma (external_forces 10ma_force_scale 10_mass_array 1_mass_array forces_10ma))
    
    ; Update 5ma
    (set! forces_5ma (external_forces 5ma_force_scale 5_mass_array 10_mass_array forces_5ma))
    (set! forces_5ma (external_forces 5ma_force_scale 5_mass_array 1_mass_array forces_5ma))
    
    ; Update 10ma
    (set! forces_1ma (external_forces 1ma_force_scale 1_mass_array 10_mass_array forces_1ma))
    (set! forces_1ma (external_forces 1ma_force_scale 1_mass_array 5_mass_array forces_1ma))
    
    ; Update arrays
    (update 10_mass_array forces_10ma t)
    (update 5_mass_array forces_5ma t)
    (update 1_mass_array forces_1ma t)

    (write_csv o 10_mass_array 5_mass_array 1_mass_array)))

(close-output-port o)