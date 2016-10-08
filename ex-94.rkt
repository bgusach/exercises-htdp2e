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
