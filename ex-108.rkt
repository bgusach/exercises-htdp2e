#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; CONSTANTS
(define BACKGROUND-WIDTH 300)
(define BACKGROUND-HEIGHT 200)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))


; DATA DEFINITIONS

; LightStatus is one of
; - red
; - green
; - green-to-red
; 
; interpretation:
;   red: light is red
;   green: light is green
;   green-to-red: light is green but showing seconds left for red


; Countdown is a Number in [0, inf)
; interpretation: amount of seconds to change status (only in time-controlled stati)


; WorldStatatus is a structure (world LightStatus Countdown)
; interpretation:
(define-struct world [status countdown])


; FUNCTIONS

; WorldState -> Image
(define (render-world ws)
  (overlay
    (cond
      [(string=? (world-status ws) "red")
       (circle 50 "solid" "red")
       ]

      [(string=? (world-status ws) "green")
       (circle 50 "solid" "green")
       ]

      [(string=? (world-status ws) "green-to-red")
       (overlay
         (text 
           (number->string (world-countdown ws)) 
           35 
           (if 
             (even? (world-countdown ws)) 
             "black" 
             "red"
             ))
         (circle 50 "solid" "green")
         )
       ])

    BACKGROUND
    ))


; WorldState KeyEvent -> WorldState
; handles the key events
(define (on-key-press ws ke)
  (if 
    (and 
      (key=? ke " ") 
      (string=? (world-status ws) "red")
      )
    (make-world
      "green"
      10
      )
    ws
    ))


; WorldState KeyEvent -> WorldState
; handles the ticking of the world
(define (tock ws)
  (cond
    [(string=? (world-status ws) "green")
     (if 
       (> (world-countdown ws) 0)
       (make-world "green" (- (world-countdown ws) 1))
       (make-world "green-to-red" 10)
       )]

    [(string=? (world-status ws) "green-to-red")
     (if 
       (> (world-countdown ws) 0)
       (make-world "green-to-red" (- (world-countdown ws) 1))
       (make-world "red" 0)
       )]
    [else ws]
    ))


(define (main ws)
  (big-bang 
    ws
    [to-draw render-world]
    [on-key on-key-press]
    [on-tick tock 1]
    ))


(main (make-world "red" 0))
