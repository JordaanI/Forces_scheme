;\\\\\\\\\\\\\  _________                           \\\\\\\\\\\\\
;\\\\\\\\\\\\\ /   _____/ ____  ____   ____   ____  \\\\\\\\\\\\\
;\\\\\\\\\\\\\\_____  \_/ ___\/ __ \ /    \_/ __ \  \\\\\\\\\\\\\
;\\\\\\\\\\\\\ /        \  \__\  ___/|   |  \  ___/ \\\\\\\\\\\\\
;\\\\\\\\\\\\\/_______  /\___  >___  >___|  /\___  >\\\\\\\\\\\\\
;\\\\\\\\\\\\\        \/     \/    \/     \/     \/ \\\\\\\\\\\\\

(include "functions.scm")
(define o (open-output-file "data.txt"))


; Initialization
(define 10_mass_array (generate_particles 100 10 500 500))
(define 5_mass_array (generate_particles 100 5 500 500))
(define 10ma_force_scale 1)
(define 5ma_force_scale 3)
(define t 0.01)

; Initialize forces
(define forces_10ma '())
(define forces_5ma '())

(do ((i 0 (+ i 1)))
    ((= 3000 i) #!void)
  (begin
    ; Update forces between particles
    (set! forces_10ma (internal_forces 10ma_force_scale 10_mass_array))
    (set! forces_5ma (internal_forces 5ma_force_scale 5_mass_array))
    (set! forces_10ma (external_forces 10ma_force_scale 10_mass_array 5_mass_array forces_10ma))
    (set! forces_5ma (external_forces 5ma_force_scale 5_mass_array 10_mass_array forces_5ma))

    ; Update arrays
    (update 10_mass_array forces_10ma t)
    (update 5_mass_array forces_5ma t)

    (write_csv o 10_mass_array 5_mass_array)))

(close-output-port o)