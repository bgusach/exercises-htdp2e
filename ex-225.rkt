#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; ### Constants
(define BACKGROUND-WIDTH 900)
(define BACKGROUND-HEIGHT 400)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))

(define AEROPLANE (rectangle 100 40 "solid" "olive"))
(define WATER (circle 20 "solid" "blue"))
(define FIRE (circle 20 "solid" "red"))


; ### Data Definitions

; WorldState is a structure (world ...)
; Interpretation:
(define-struct world [x])


; ### Functions

; WorldState -> Image
(define (render-world ws)
  (place-image
    (beside
      AEROPLANE
      WATER
      FIRE
      )
    100
    100
    BACKGROUND
    ))


; WorldState KeyEvent -> WorldState
; Handles the key events
(define (on-key-press ws ke)
  (cond
    [(key=? ke "...") ws]
    [else ws]
    ))


; WorldState -> WorldState
; Handles the ticking of the world
(define (tock ws)
  ws
  )


; WorldState -> Boolean
; Predicate to define when the world comes to an end
(define (over? ws)
  #false
  )


; WorldState -> Image
; Renders the last image after the world ended
(define (render-final ws)
  (render-world ws)
  )


(define (main ws)
  (big-bang 
    ws
    [to-draw render-world]
    [on-key on-key-press]
    [on-tick tock]
    [stop-when over? render-final]
    [check-with world?]
    ))


(test)
(main (make-world #false))

