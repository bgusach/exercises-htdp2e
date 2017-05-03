#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; ### Constants
(define BACKGROUND-WIDTH 900)
(define BACKGROUND-HEIGHT 400)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))

(define AEROPLANE-SPEED 5)
(define AEROPLANE (rectangle 100 40 "solid" "olive"))
(define WATER (circle 20 "solid" "blue"))
(define WATER-SPEED 10)
(define FIRE (circle 20 "solid" "red"))


; ### Data Definitions

; Direction is one of:
; - "right"
; - "left"

; Aeroplane is a structure (make-aeroplane x y direction)
(define-struct plane [x y direction])


; Fire is a Posn, representing its position

; Fires is one of:
; - '()
; - (cons Fire Fires)

; Water is a Posn, representing its position

; Waters is one of:
; - '()
; - (cons Water Waters)

; WorldState is a structure (make-world Aeroplane Fires Waters)
(define-struct world [plane fires waters])


; ### Functions

; WorldState -> Image
(define (render-world ws)
  (render-plane
    (world-plane ws)
    BACKGROUND
    ))


; Aeroplane Image -> Image
; Renders the plane on top of img
(define (render-plane plane img)
  (place-image
    AEROPLANE
    (plane-x plane)
    img
    ))


; WorldState KeyEvent -> WorldState
; Handles the key events
(define (on-key-press ws ke)
  (make-world
    (world-plane )
    (world-fires ws)
    (world-waters ws)
    )
    [(key=? ke "left") ws]


; WorldState -> WorldState
; Handles the ticking of the world
(define (tock ws)
  (make-world
    (plane-tock (world-plane ws))
    (world-fires ws)
    (world-waters ws)
    ))


(define (plane-tock plane)
  (make-plane
    (+ 
      (cond
        [(string=? (plane-direction plane) "right") AEROPLANE-SPEED]
        [(string=? (plane-direction plane) "left") (* -1 AEROPLANE-SPEED)]
        )
      (plane-x plane)
      )
    (plane-y plane)
    (plane-direction plane)
    ))


; Posn Number Number -> Posn
(define (translate-posn posn x y)
  (make-posn
    (+ (posn-x posn) x)
    (+ (posn-y posn) y)
    ))

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
(main 
  (make-world 
    (make-plane 50 50 "right")
    '()
    '()
    ))

