#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

(define BACKGROUND-W 500)
(define BACKGROUND-H 200)

(define GROUND-H (/ BACKGROUND-H 3))
(define GROUND (rectangle BACKGROUND-W GROUND-H "solid" "khaki"))

(define BACKGROUND 
  (overlay/align
    "middle"
    "bottom"
    GROUND
    (rectangle BACKGROUND-W BACKGROUND-H "solid" "skyblue")))

(define TANK-W 50)
(define TANK-H 15)
(define TANK (rectangle TANK-W TANK-H "solid" "blue"))
(define TANK-MAX-SPEED 7)
(define TANK-ACC 1)

(define UFO-W 30)
(define UFO-H 5)
(define UFO-R 10)
(define UFO-SPEED 1)
(define UFO-DRIFT 5)
(define UFO
  (overlay
    (rectangle UFO-W UFO-H "solid" "green")
    (circle UFO-R "solid" "olive")))

(define MISSILE-H 15)
(define MISSILE (triangle MISSILE-H "solid" "red"))
(define MISSILE-SPEED -10)

; sUFO is a Posn
; interpretation (make-posn x y): UFO's location

(define-struct tank [pos vel])
; a Tank is a structure: (make-tank Number Number)
; interpretation (make-tank x dx): 
;   position (x, BACKGROUND-H)
;   speed dx pixels/tick

; a MaybeMissile is one of the following
; - #false 
; - Posn
; interepretation: #false means the missile has not been fired yet
; whereas a Posn represents the position of a fired missile

(define (missile? missile)
  (posn? missile))


; SIGS (space invaders game state) is a structure
; (make-sigs UFO Tank MaybeMissile)
; interpretation: represents the complete state of the game
(define-struct sigs [ufo tank missile])


; SIGS -> Image
; Adds TANK, UFO and possibly a MISSILE to the BACKGROUND
(define (render-world ws)
  (render-ufo 
    (sigs-ufo ws)
    (render-tank
      (sigs-tank ws)
      (render-missile
        (if 
          (missile? (sigs-missile ws))
          (sigs-missile ws)
          (stand-by-missile (sigs-tank ws)))
        BACKGROUND))))


; Tank -> Missile
; Calculates the missile position from a tank when it has not been fired yet
(define (stand-by-missile tank)
  (make-posn 
    (tank-pos tank)
    (- BACKGROUND-H TANK-H 15)))


; Tank Image -> Image
; adds a tank image to the passed image
(define (render-tank tank img)
  (place-image 
    TANK
    (tank-pos tank)
    (- BACKGROUND-H TANK-H)
    img))


; sUFO Image -> Image
; adds a ufo image to the passed image
(define (render-ufo ufo img)
  (place-image
    UFO
    (posn-x ufo)
    (posn-y ufo)
    img))


; Missile Image -> Image
; adds a missile image to the passed image
(define (render-missile mis img)
  (place-image
    MISSILE
    (posn-x mis)
    (posn-y mis)
    img))

; SIGS -> Image
; renders the frame when the game is over
(define (si-render-final ws)
  (place-image/align
    (text "END!" 50 "olive")
    (/ BACKGROUND-W 2)
    (/ BACKGROUND-H 2)
    "center"
    "center"
    (render-world ws)))


; SIGS -> Boolean
; Returns whether the game is over
; If the UFO has landed or the missile has hit the UFO, the game is over
(define (si-game-over ws)
  (or
     (ufo-landed (sigs-ufo ws))
     (missile-hitting (sigs-missile ws) (sigs-ufo ws))))

; sUFO -> Boolean
; Returns whether the UFO has landed
(define (ufo-landed ufo)
  (>= (posn-y ufo) BACKGROUND-H))


; sUFO Missile -> Boolean
; Returns whether the missile is hitting the UFO
(define (missile-hitting missile ufo)
  (and 
    (missile? missile)
    (<= (euc-distance missile ufo) UFO-R)))


; Posn Posn -> Number
; Returns the euclidean distance between two points
(define (euc-distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))


; SIGS -> SIGS
(define (si-move ws)
  (make-sigs
    (move-ufo (sigs-ufo ws))
    (move-tank (sigs-tank ws))
    (move-missile (sigs-missile ws))))


; MaybeMissile -> MaybeMissile
(define (move-missile missile)
  (if
    (missile? missile)
    (move-posn missile 0 MISSILE-SPEED)
    #false))


; sUFO -> sUFO
(define (move-ufo ufo)
  (make-posn
    (+ 
      (posn-x ufo)
      (*
        (if (= (random 2) 0) 1 -1)
        (random UFO-DRIFT)))
    (+ 
      (posn-y ufo) 
      UFO-SPEED)))


; Tank -> Tank
; computes the new position of the tank according to its speed
(check-expect (move-tank (make-tank 0 5)) (make-tank 5 (- 5 TANK-ACC)))
(define (move-tank tank)
  (make-tank
    (+ (tank-pos tank) (tank-vel tank))
    (cond
      [(< (tank-vel tank) 0) (+ (tank-vel tank) TANK-ACC)]
      [(> (tank-vel tank) 0) (- (tank-vel tank) TANK-ACC)]
      [else (tank-vel tank)])))

; Posn -> Posn
; Adds the offsets x and y to the Posn and returns a new one
(check-expect 
  (move-posn (make-posn 1 2) 4 -3)
  (make-posn 5 -1))
(define (move-posn p x y)
  (make-posn 
    (+ (posn-x p) x)
    (+ (posn-y p) y)))


; SIGS key-event? -> SIGS
; handles the key events of the SI game
(define (si-control ws ke)
  (cond
    [(and (key=? ke " ") (not (missile? (sigs-missile ws))))
     (make-sigs
       (sigs-ufo ws)
       (sigs-tank ws)
       (stand-by-missile (sigs-tank ws)))]

    [(key=? ke "left") 
       (make-sigs
         (sigs-ufo ws)
         (make-tank
           (tank-pos (sigs-tank ws))
           (change-sign TANK-MAX-SPEED))
         (sigs-missile ws))]

    [(key=? ke "right") 
       (make-sigs
         (sigs-ufo ws)
         (make-tank
           (tank-pos (sigs-tank ws))
           TANK-MAX-SPEED)
         (sigs-missile ws))]

    [else ws]))


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
(main (make-sigs (make-posn (random BACKGROUND-W) 20) (make-tank 200 0) #false))
