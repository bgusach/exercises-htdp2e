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
; interpretation (make-posn x y): missile location


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
    40
    (- BACKGROUND-H TANK-H)
    (place-image
      MISSILE
      40
      (- BACKGROUND-H TANK-H MISSILE-H)
      (place-image
        UFO
        60
        60
        BACKGROUND))))


(define (main world)
  (big-bang 
    world
    [to-draw render-world]))

(main "fake world")
