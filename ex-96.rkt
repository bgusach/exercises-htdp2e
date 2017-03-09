#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

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

(define UFO-W 30)
(define UFO-H 5)
(define UFO-R 10)
(define UFO
  (overlay
    (rectangle UFO-W UFO-H "solid" "green")
    (circle UFO-R "solid" "olive")))

(define MISSILE-H 15)
(define MISSILE (triangle MISSILE-H "solid" "red"))

; sUFO is a Posn
; interpretation (make-posn x y): UFO's location

(define-struct tank [loc vel])
; a Tank is a structure: (make-tank Number Number)
; interpretation (make-tank x dx): 
;   location (x, BACKGROUND-H)
;   speed dx pixels/tick

; a Missile is a Posn
; interepretation (make-posn x y): missile location


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
(define (render-world world)
  (place-image 
    TANK
    (tank-loc (tank-from-world world))
    (- BACKGROUND-H TANK-H)
    (place-image
      MISSILE
      ; This repetition is annoying: waiting for some kind of let...
      (posn-x (missile-location world))
      (posn-y (missile-location world))
      (place-image
        UFO
        (posn-x (ufo-from-world world))
        (posn-y (ufo-from-world world))
        BACKGROUND))))


; SIGS -> make-posn
(define (missile-location world)
  (cond 
    [(fired? world) (fired-missile world)]
    [else (make-posn 
            (tank-loc (aim-tank world))
            (- BACKGROUND-H TANK-H MISSILE-H))]))


; SIGS -> make-tank
(define (tank-from-world world)
  ; I hope there are more polymorphic ways of doing this... 
  (cond 
    [(fired? world) (fired-tank world)]
    [else (aim-tank world)]))

; SIGS -> sUFO
(define (ufo-from-world world)
  (cond
    [(fired? world) (fired-ufo world)]
    [else (aim-ufo world)]))


(define (main world)
  (big-bang 
    world
    [to-draw render-world]))

(main (make-aim (make-posn 30 70) (make-tank 200 0)))  ; still aiming
(main (make-fired (make-posn 10 10) (make-tank 50 0) (make-posn 60 60))) ; already fired
