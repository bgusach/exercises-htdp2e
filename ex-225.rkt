#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; ### Constants
(define BACKGROUND-WIDTH 900)
(define BACKGROUND-HEIGHT 400)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))

(define AEROPLANE-SPEED 10)
(define AEROPLANE-WIDTH 100)
(define AEROPLANE (rectangle AEROPLANE-WIDTH 40 "solid" "olive"))
(define AEROPLANE-Y 100)
(define WATER (circle 20 "solid" "blue"))
(define WATER-SPEED 10)
(define FIRE (circle 20 "solid" "red"))
(define FIRE-Y (- BACKGROUND-HEIGHT 10))
(define COLLISION-DISTANCE 30)


; ### Data Definitions

; Direction is one of:
; - "right"
; - "left"
; or one of:
(define DIRECTIONS (list "right" "left"))

; Aeroplane is a structure (make-aeroplane x direction)
(define-struct plane [x direction])


; Fire is a Number representing its x-position

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
    (render-fires
      (world-fires ws)
      (render-waters 
        (world-waters ws)
        BACKGROUND
        ))))


; Aeroplane Image -> Image
; Renders the plane on top of img
(define (render-plane plane img)
  (place-image
    AEROPLANE
    (plane-x plane)
    AEROPLANE-Y
    img
    ))


; Waters Image -> Image
; Renders the waters on top of the image
(define (render-waters waters img)
  (cond
    [(empty? waters) img]
    [else
      (place-image
        WATER
        (posn-x (first waters))
        (posn-y (first waters))
        (render-waters (rest waters) img)
        )]))


; Waters Image -> Image
; Renders the fires on top of the image
(define (render-fires fires img)
  (cond
    [(empty? fires) img]
    [else
      (place-image
        FIRE
        (first fires)
        FIRE-Y
        (render-fires (rest fires) img)
        )]))


; WorldState KeyEvent -> WorldState
; Handles the key events
(define (on-key-press ws ke)
  (cond
    [(key=? ke " ") (add-water ws)]
    [else ws]
    ))


; WorldState -> WorldState
; Throws a unit of water
(define (add-water ws)
  (make-world
    (world-plane ws)
    (world-fires ws)
    (cons
      (make-posn (plane-x (world-plane ws)) AEROPLANE-Y)
      (world-waters ws)
      )))


; WorldState -> WorldState
; Handles the ticking of the world
(define (tock ws)
  (tock-helper 
    (world-plane ws)
    (world-fires ws)
    (world-waters ws)
    ))


(define (tock-helper plane fires waters)
  (make-world
    (plane-tock plane)
    (fires-tocks fires waters)
    (waters-tock waters)
    ))


(define (waters-tock waters)
  (cond
    [(empty? waters) '()]

    [(< (posn-y (first waters)) BACKGROUND-HEIGHT)
      (cons
        (translate-posn (first waters) 0 WATER-SPEED)
        (waters-tock (rest waters))
        )]

    [else (waters-tock (rest waters))]
    ))


; Fires Waters -> Fires
; Returns the list of fires that do not collide with any water
(define (fires-tocks fires waters)
  (cond
    [(empty? fires) '()]

    [(fire-waters-collide? (first fires) waters) 
     (fires-tocks (rest fires) waters)
     ]

    [else 
      (cons
        (first fires)
        (fires-tocks (rest fires) waters)
        )]))


; Fire Waters -> Boolean
; Returns whether the fire is being hit by any water
(define (fire-waters-collide? fire waters)
  (cond
    [(empty? waters) #false]
    [else
      (or
        (fire-water-collide? fire (first waters))
        (fire-waters-collide? fire (rest waters))
        )]))


; Fire Water -> Boolean
; Returns whether the water is hitting the fire
(check-expect 
  (fire-water-collide? 0 (make-posn COLLISION-DISTANCE FIRE-Y))
  #true
  )
(check-expect 
  (fire-water-collide? 100 (make-posn (+ COLLISION-DISTANCE 100) FIRE-Y))
  #true
  )
(check-expect 
  (fire-water-collide? 100 (make-posn (+ COLLISION-DISTANCE 100 1) FIRE-Y))
  #false
  )
(check-expect 
  (fire-water-collide? 0 (make-posn 100 COLLISION-DISTANCE))
  #false
  )
(define (fire-water-collide? fire water)
  (<=
    (sqrt 
      (+
        (sqr (- fire (posn-x water)))
        (sqr (- FIRE-Y (posn-y water)))
        ))
    COLLISION-DISTANCE
    ))


; Plane -> Plane
; Handles the tick of the plane
(define (plane-tock plane)
  (plane-tock-helper
    (plane-x plane)
    (plane-direction plane)
    ))


; Same as plane-tock, but with bound names
(define (plane-tock-helper x direction)
  (make-plane
    (cond
      [(string=? direction "right") 
       (if
         (< x BACKGROUND-WIDTH)
         (+ x AEROPLANE-SPEED)
         (* -1 AEROPLANE-WIDTH)
         )]

      [(string=? direction "left") 
       (if
         (> x (* -1 AEROPLANE-WIDTH))
         (- x AEROPLANE-SPEED)
         BACKGROUND-WIDTH
         )])

    direction
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
    (make-plane 0 "right")
    (list 450)
    '()
    ))

