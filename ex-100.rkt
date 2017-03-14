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

; a Missile is a Posn
; interepretation (make-posn x y): missile at (x, y)


(define-struct aim [ufo tank])
; an Aim is a structure (make-aim sUFO Tank)
; interpretation: state of the ufo and the tank, prior to firing the missile


(define-struct fired [ufo tank missile])
; a Fired is a structure (make-fired sUFO Tank Missile)
; interpretation: state of the world with ufo, tank and fired missile


; SIGS (space invaders game state) is one of:
;   - (make-aim sUFO Tank)
;   - (make-fired sUFO Tank Missile)
; interpretation: represents the complete state of the game


; SIGS -> Image
; Adds TANK, UFO and possibly a MISSILE to the BACKGROUND
(define (render-world world)
  (cond
    [(aim? world) (render-aim-world world)]
    [(fired? world) (render-fired-world world)]))


; make-aim -> Image
; creates an image for an aim? data-structure
(define (render-aim-world ws)
  (render-ufo 
    (aim-ufo ws)
    (render-tank
      (aim-tank ws)
      (render-missile
        (stand-by-missile (aim-tank ws))
        BACKGROUND))))


; Tank -> Missile
; Calculates the missile position from a tank when it has not been fired yet
(define (stand-by-missile tank)
  (make-posn 
    (tank-pos tank)
    (- BACKGROUND-H TANK-H 15)))


; fired? -> Image
; creates an image for a world in status make-fired
(define (render-fired-world ws)
  (render-ufo 
    (fired-ufo ws)
    (render-tank
      (fired-tank ws)
      (render-missile
        (fired-missile ws)
        BACKGROUND))))


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
(check-expect 
  (si-game-over
    (make-aim (make-posn 0 0) (make-tank 0 0))) 
  #false)
(check-expect 
  (si-game-over
    (make-aim (make-posn 0 BACKGROUND-H) (make-tank 0 0))) 
  #true)
(define (si-game-over ws)
  (cond
    [(aim? ws) 
     (ufo-landed (aim-ufo ws))]

    [(fired? ws) 
     (or 
       (ufo-landed (fired-ufo ws))
       (missile-hitting (fired-missile ws) (fired-ufo ws)))]))


; sUFO -> Boolean
; Returns whether the UFO has landed
(define (ufo-landed ufo)
  (>= (posn-y ufo) BACKGROUND-H))


; sUFO Missile -> Boolean
; Returns whether the missile is hitting the UFO
(define (missile-hitting missile ufo)
  (<= (euc-distance missile ufo)
      UFO-R))


; Posn Posn -> Number
; Returns the euclidean distance between two points
(define (euc-distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))


; SIGS -> SIGS
(define (si-move ws)
  (cond 
    [(aim? ws) 
     (make-aim 
       (move-ufo (aim-ufo ws))
       (move-tank (aim-tank ws)))]

    [(fired? ws)
     (make-fired
       (move-ufo (fired-ufo ws))
       (move-tank (fired-tank ws))
       (move-posn (fired-missile ws) 0 MISSILE-SPEED))]))


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
    [(and (aim? ws) (key=? ke " ")) 
     (make-fired
       (aim-ufo ws)
       (aim-tank ws)
       (stand-by-missile (aim-tank ws)))]

    [(aim? ws) 
     (make-aim 
       (aim-ufo ws) 
       (control-tank (aim-tank ws) ke))]

    [(fired? ws) 
     (make-fired 
       (fired-ufo ws) 
       (control-tank (fired-tank ws) ke)
       (fired-missile ws))]))


; Tank KeyEvent -> Tank
; given a Tank and a KeyEvent, it resolves the speed of the tank
(check-expect 
  (control-tank (make-tank 0 5) "left")
  (make-tank 0 (change-sign TANK-MAX-SPEED)))
(check-expect 
  (control-tank (make-tank 0 -9) "right")
  (make-tank 0 TANK-MAX-SPEED))
(check-expect 
  (control-tank (make-tank 0 0) " ")
  (make-tank 0 0))
(define (control-tank tank ke)
  (make-tank
    (tank-pos tank)
    (cond
      [(key=? ke "left") (change-sign TANK-MAX-SPEED)]
      [(key=? ke "right") TANK-MAX-SPEED]
      [else (tank-vel tank)])))


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
(main (make-aim (make-posn (random BACKGROUND-W) 20) (make-tank 200 0)))
