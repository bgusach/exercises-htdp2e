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
(define WATER-CAPACITY 5)
(define WATER (circle 20 "solid" "blue"))
(define WATER-SPEED 10)
(define WATER-THUMBNAIL 
  (overlay
    (circle 8 "solid" "blue")
    (circle 10 "solid" "transparent")
    ))
(define FIRE (circle 20 "solid" "red"))
(define FIRE-Y (- BACKGROUND-HEIGHT 10))
(define FIRE-PROPAGATION-PROBABILITY 1)  ; 1% per tick
(define INITIAL-FIRE-DISTANCE 30)
(define COLLISION-DISTANCE 30)


; ### Data Definitions

; Direction is one of:
; - "right"
; - "left"
; or one of:
(define DIRECTIONS (list "right" "left"))

; Aeroplane is a structure (make-aeroplane x direction deposit)
; Interpretation: aeroplane is at x-position, moving towards direction
; and has a water amount in its deposit
(define-struct plane [x direction water])


; Fire is a structure (make-fire x h)
; Interpretation: position and height of fire
(define-struct fire [x h])

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
        (render-info
          ws
          BACKGROUND
          )))))


; WorldState Image -> Image
; Renders some game information
(define (render-info ws img)
  (overlay/align
    "left"
    "top"
    ; Variadic, add as many as you want
    (render-water-tank (plane-water (world-plane ws)))
    img
    ))


; PositiveNumber -> Image
(define (render-water-tank n)
  (cond
    [(zero? n) empty-image]
    [else
      (beside WATER-THUMBNAIL (render-water-tank (sub1 n)))
      ]))


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
      (place-image/align
        (render-fire (fire-h (first fires)))
        (fire-x (first fires))
        FIRE-Y
        "middle"
        "bottom"
        (render-fires (rest fires) img)
        )]))


(define (render-fire height)
  (cond
    [(zero? height) empty-image]
    [else
      (above 
        FIRE 
        (render-fire (sub1 height))
        )]))


; WorldState KeyEvent -> WorldState
; Handles the key events
(define (on-key-press ws ke)
  (cond
    [(key=? ke " ") (add-water ws)]
    [else ws]
    ))


; WorldState -> WorldState
; Throws a unit of water if possible
(define (add-water ws)
  (if 
    (positive? (plane-water (world-plane ws)))
    (make-world
      (subtract-plane-water (world-plane ws))
      (world-fires ws)
      (cons
        (make-posn (plane-x (world-plane ws)) AEROPLANE-Y)
        (world-waters ws)
        ))
    ws
    ))


; Plane -> Plane
(define (subtract-plane-water plane)
  (make-plane
    (plane-x plane)
    (plane-direction plane)
    (sub1 (plane-water plane))
    ))


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


; Waters -> Waters
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


(define (fires-tocks fires waters)
  (reproduce-fires
    (extinguish-fires fires waters)
    ))


; Fires Waters -> Fires
; Returns the list of fires that do not collide with any water
(define (extinguish-fires fires waters)
  (cond
    [(empty? fires) '()]

    [(fire-waters-collide? (first fires) waters) 
     (extinguish-fires (rest fires) waters)
     ]

    [else 
      (cons
        (first fires)
        (extinguish-fires (rest fires) waters)
        )]))


(define (reproduce-fires fires)
  (if
    (empty? fires)
    (make-random-fires 5)
    (propagate-fires fires)
    ))


; Fires -> Fires
(define (propagate-fires fires)
  (cond
    [(empty? fires) '()]
    [else
      (cons
        (make-fire
          (fire-x (first fires))
          (+
            (fire-h (first fires))
            (if
              (<= (random 100) FIRE-PROPAGATION-PROBABILITY)
              1
              0
              )))
        (propagate-fires (rest fires))
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
  (fire-water-collide? 
    (make-fire 0 #false) 
    (make-posn COLLISION-DISTANCE FIRE-Y)
    )
  #true
  )
(check-expect 
  (fire-water-collide? 
    (make-fire 100 #false) 
    (make-posn (+ COLLISION-DISTANCE 100) FIRE-Y)
    )
  #true
  )
(check-expect 
  (fire-water-collide? 
    (make-fire 100 #false) 
    (make-posn (+ COLLISION-DISTANCE 100 1) FIRE-Y)
    )
  #false
  )
(check-expect 
  (fire-water-collide? 
    (make-fire 0 #false) 
    (make-posn 100 COLLISION-DISTANCE)
    )
  #false
  )
(define (fire-water-collide? fire water)
  (<=
    (sqrt 
      (+
        ; TODO: use fire-height to detect collision
        (sqr (- (fire-x fire) (posn-x water)))
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
    (plane-water plane)
    ))


; Same as plane-tock, but with bound names
(define (plane-tock-helper x direction water)
  (cond
    [(> x (+ BACKGROUND-WIDTH AEROPLANE-WIDTH))
     (make-plane
       (sub1 (+ BACKGROUND-WIDTH AEROPLANE-WIDTH))
       "left"
       WATER-CAPACITY
       )]

    [(< x (* -1 AEROPLANE-WIDTH))
     (make-plane
       (add1 (* -1 AEROPLANE-WIDTH))
       "right"
       WATER-CAPACITY
       )]

    [else
      (make-plane
        (if 
          (string=? direction "right")
          (+ x AEROPLANE-SPEED)
          (- x AEROPLANE-SPEED)
          )
        direction
        water
        )]))


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


; Number -> Fires
; Generates n random fires
(define (make-random-fires n)
  (make-random-no-collision-fires n '())
  )


; Number Fires -> Fires
(define (make-random-no-collision-fires n existing-fires)
  (cond 
    [(zero? n) '()]
    [else
      (cons
        (make-random-no-collision-fire existing-fires)
        (make-random-no-collision-fires (sub1 n) existing-fires)
        )]))


; Fires -> Fire
(define (make-random-no-collision-fire existing-fires)
  (check-fire 
    (make-fire (random BACKGROUND-WIDTH) 1) 
    existing-fires
    ))


; Fire Fires -> Fire
(define (check-fire fire existing-fires)
  (if 
    (fire-collision? fire existing-fires)
    (make-random-no-collision-fire existing-fires)
    fire
    ))


; Fire Fires -> Boolean
(check-expect (fire-collision? (make-fire 50 0) '()) #false)
(check-expect 
  (fire-collision? 
    (make-fire INITIAL-FIRE-DISTANCE #false)
    (list 
      (make-fire 0 0) 
      (make-fire (sub1 INITIAL-FIRE-DISTANCE) 0)
      ))
  #true
  )
(define (fire-collision? fire existing-fires)
  (cond
    [(empty? existing-fires) #false]
    [else
      (or
        (<= 
          (abs (- (fire-x (first existing-fires)) (fire-x fire)))
          INITIAL-FIRE-DISTANCE
          )
        (fire-collision? fire (rest existing-fires))
        )]))


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
    (make-plane 0 "right" WATER-CAPACITY)
    (make-random-fires 5)
    '()
    ))

