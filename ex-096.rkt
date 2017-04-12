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
        ; Calculate missile position from tank position
        (make-posn 
          (tank-loc (aim-tank ws))
          (- BACKGROUND-H TANK-H 15))
        BACKGROUND))))


; make-fired -> Image
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
    (tank-loc tank)
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


(define (main world)
  (big-bang 
    world
    [to-draw render-world]))


(main (make-aim (make-posn 30 70) (make-tank 200 0)))  ; still aiming
(main (make-fired (make-posn 10 10) (make-tank 50 0) (make-posn 60 60))) ; already fired
