#lang htdp/isl

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)


; ### Constants
(define BACKGROUND-W 500)
(define BACKGROUND-H 500)

(define GROUND-H (/ BACKGROUND-H 3))
(define GROUND (rectangle BACKGROUND-W GROUND-H "solid" "khaki"))

(define BACKGROUND 
  (overlay/align
    "middle"
    "bottom"
    GROUND
    (rectangle BACKGROUND-W BACKGROUND-H "solid" "skyblue")
    ))

(define MISSILE-H 15)
(define MISSILE (triangle MISSILE-H "solid" "red"))
(define MISSILE-SPEED -10)

(define TANK-W 50)
(define TANK-H 15)
(define TANK-MAX-SPEED 7)
(define TANK-ACC 1)
(define 
  TANK 
  (above
    MISSILE
    (rectangle TANK-W TANK-H "solid" "blue")
    ))

(define TANK-Y (- BACKGROUND-H TANK-H))

(define UFO-W 30)
(define UFO-H 5)
(define UFO-R 10)
(define UFO-SPEED 2)
(define UFO-DRIFT 8)
(define UFO
  (overlay
    (rectangle UFO-W UFO-H "solid" "green")
    (circle UFO-R "solid" "olive")
    ))

(define CHARGE-R 7)
(define CHARGE (circle CHARGE-R "solid" "blue"))
(define CHARGE-SPEED 7)
(define CHARGE-SHOOT-PROBABILITY 30)  ; 1 out of ... ticks


; ### Data Definitions

; sUFO is a Posn
; interpretation (make-posn x y): UFO's location

(define-struct tank [pos vel])
; a Tank is a structure: (make-tank Number Number)
; interpretation (make-tank x dx): 
;   position (x, BACKGROUND-H)
;   speed dx pixels/tick


; A Missile is a Posn
; A Charge is a Posn


; SIGS (space invaders game state) is a structure
; (make-sigs UFO Tank [List-of Missile] [List-of Charge])
; interpretation: represents the possitions of the ufo,
; the tank, the missiles (shot by the tank) and the
; charges (shot by the ufo)
(define-struct sigs [ufo tank missiles charges])


; ### Functions

; SIGS -> Image
(define (render-world ws)
  (render-ufo 
    (sigs-ufo ws)
    (render-tank
      (sigs-tank ws)
      (render-missiles
        (sigs-missiles ws)
        (render-charges
          (sigs-charges ws)
          BACKGROUND
          )))))


; Calculates the missile position from a tank when it has not been fired yet
(define (stand-by-missile tank)
  (make-posn 
    (tank-pos tank)
    (- BACKGROUND-H TANK-H 15)
    ))


; Tank Image -> Image
; adds a tank image to the passed image
(define (render-tank tank img)
  (place-image 
    TANK
    (tank-pos tank)
    TANK-Y
    img
    ))


; sUFO Image -> Image
; adds a ufo image to the passed image
(define (render-ufo ufo img)
  (place-image UFO (posn-x ufo) (posn-y ufo) img)
  )


; Missile Image -> Image
; Renders the missile on top of the image
(define (render-missile mis img)
  (place-image MISSILE (posn-x mis) (posn-y mis) img)
  )


; MissileList Image -> Image
; adds missiles image to the passed image
(define (render-missiles missiles img)
  (foldl render-missile img missiles)
  )


; Charge Image -> Image
; Renders charge on top of img
(define (render-charge charge img)
 (place-image CHARGE (posn-x charge) (posn-y charge) img)
 )


; [List-of Charge] Image -> Image
(define (render-charges charges img)
  (render-posns render-charge charges img)
  )


; [Posn -> Image] [List-of Posn] -> Image
; Given a posns list and a function to render each, it renders
; all of them on top of img
(define (render-posns render-posn posns img)
  (local
    (; Posn Image -> Image
     (define (reduce posn img)
       (render-posn posn img)
       ))

    ; -- IN --
    (foldl reduce img posns)
    ))


; SIGS -> Image
; renders the frame when the game is over
(define (si-render-final ws)
  (place-image/align
    (text "END!" 50 "olive")
    (/ BACKGROUND-W 2)
    (/ BACKGROUND-H 2)
    "center"
    "center"
    (render-world ws)
    ))


; SIGS -> Boolean
; Returns whether the game is over
(define (si-game-over ws)
  (or
     (ufo-landed (sigs-ufo ws))
     (any-missile-hitting? (sigs-missiles ws) (sigs-ufo ws))
     (any-charge-hitting? (sigs-charges ws) (sigs-tank ws))
     ))


; sUFO -> Boolean
; Returns whether the UFO has landed
(define (ufo-landed ufo)
  (>= (posn-y ufo) BACKGROUND-H)
  )


; sUFO Missile -> Boolean
; Returns whether the missile is hitting the UFO
(check-expect (any-missile-hitting? '() (make-posn 0 0)) #false)
(check-expect 
  (any-missile-hitting? (list (make-posn 0 0) (make-posn 5 5)) (make-posn 5 5))
  #true
  )
(define (any-missile-hitting? missiles ufo)
  (local
    (; Missile -> Boolean
     ; Returns whether the missile is hitting the ufo
     (define (hitting-ufo? missile)
       (<= (euc-distance missile ufo) UFO-R)
       ))

    ; -- IN --
    (ormap hitting-ufo? missiles)
    ))

; [List-of Charge] Tank -> Boolean
(define (any-charge-hitting? charges tank)
  (local
    (; Charge -> Boolean
     ; Returns whether the charge is hitting the tank
     (define (hitting-tank? charge)
       (<= 
         (euc-distance 
           charge
           (make-posn (tank-pos tank) TANK-Y)
           ) 
         TANK-H
         )))

    ; -- IN --
    (ormap hitting-tank? charges)
    ))


; UFO [List-of Posn] -> [List-of Posn]
; Moves charges, and shoots another one
; once in a while
(define (handle-charges ufo charges)
  (local
    ((define 
      new-charges
      (if
        (zero? (random CHARGE-SHOOT-PROBABILITY))
        (cons (make-posn (posn-x ufo) (posn-y ufo)) charges)
        charges
        )))

    ; -- IN --
    (translate-posns new-charges 0 CHARGE-SPEED)
    ))


; Posn Posn -> Number
; Returns the euclidean distance between two points
(define (euc-distance p1 p2)
  (sqrt 
    (+ 
      (sqr (- (posn-x p1) (posn-x p2)))
      (sqr (- (posn-y p1) (posn-y p2)))
      )))


; SIGS -> SIGS
(define (si-move ws)
  (make-sigs
    (move-ufo (sigs-ufo ws))
    (move-tank (sigs-tank ws))
    (move-missiles (sigs-missiles ws))
    (handle-charges (sigs-ufo ws) (sigs-charges ws))
    ))


; [List-of Posn] Number Number -> [List-of Posn]
; Translates the posns by x, y
(define (translate-posns posns x y)
  (local
    (; Posn -> Posn
     ; Moves posn by x and y
     (define (translate posn)
       (move-posn posn x y)
       ))

    ; -- IN --
    (map translate posns)
    ))

; [List-of Missile] -> [List-of Missile]
; moves the list of missiles
(check-expect 
  (move-missiles (list (make-posn 0 0)))
  (list (make-posn 0 MISSILE-SPEED))
  )
(define (move-missiles missiles)
  (translate-posns missiles 0 MISSILE-SPEED)
  )


; sUFO -> sUFO
(define (move-ufo ufo)
  (move-posn
    ufo
    (* (if (= (random 2) 0) 1 -1) (random UFO-DRIFT))
    UFO-SPEED
    ))


; Tank -> Tank
; computes the new position of the tank according to its speed
(check-expect (move-tank (make-tank 0 5)) (make-tank 5 (- 5 TANK-ACC)))
(define (move-tank tank)
  (make-tank
    (+ (tank-pos tank) (tank-vel tank))
    (cond
      [(< (tank-vel tank) 0) (+ (tank-vel tank) TANK-ACC)]
      [(> (tank-vel tank) 0) (- (tank-vel tank) TANK-ACC)]
      [else (tank-vel tank)]
      )))


; Posn Number Number -> Posn
; Adds the offsets x and y to the Posn and returns a new one
(check-expect 
  (move-posn (make-posn 1 2) 4 -3)
  (make-posn 5 -1))
(define (move-posn p x y)
  (make-posn 
    (+ (posn-x p) x)
    (+ (posn-y p) y)
    ))


; SIGS key-event? -> SIGS
; handles the key events of the SI game
(define (si-control ws ke)
  (cond
    [(key=? ke " ") 
     (make-sigs
       (sigs-ufo ws)
       (sigs-tank ws)
       (cons
         (stand-by-missile (sigs-tank ws))
         (sigs-missiles ws)
         )
       (sigs-charges ws)
       )]

    [(key=? ke "left") 
       (make-sigs
         (sigs-ufo ws)
         (make-tank
           (tank-pos (sigs-tank ws))
           (change-sign TANK-MAX-SPEED))
         (sigs-missiles ws)
         (sigs-charges ws)
         )]

    [(key=? ke "right") 
       (make-sigs
         (sigs-ufo ws)
         (make-tank
           (tank-pos (sigs-tank ws))
           TANK-MAX-SPEED)
         (sigs-missiles ws)
         (sigs-charges ws)
         )]

    [else ws]
    ))


; Number -> Number
; changes the sign of the passed number
(check-expect (change-sign 3) -3)
(check-expect (change-sign -8) 8)
(define (change-sign num)
  (* -1 num))


(define (main world)
  (big-bang 
    world
    [to-draw render-world]
    [stop-when si-game-over si-render-final]
    [on-tick si-move]
    [on-key si-control]))


(test)
(main 
  (make-sigs 
    (make-posn (random BACKGROUND-W) 20) 
    (make-tank 200 0) 
    '()
    '()
    ))
