#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)


(define cat-image (bitmap "../images/cat.png"))
(define BACKGROUND-WIDTH 300)
(define BACKGROUND-HEIGHT 180)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))
(define CAT-SPEED 3)
(define HAPPINESS-SPEED -1)


; A Direction is one of
; - "right"
; - "left"
; interpretation: direction to which the cat is moving

; A Position is a Number in [0, 100]
; interpretation: 0 leftmost position 100 rightmost position

; A Happiness is a Number in [0, 100]
; interpretation: 0 minimum happiness, 100 maximum


(define-struct cat [pos direction happiness])
; Cat is (make-cat Position Direction Happiness)
; intepretation: state of the world defined by
; where is the cat, what direction is following, and how happy it is


; Cat -> Image
(define (render-world ws)
  (place-image
    (get-cat-image ws)
    (cat-pos ws)
    (/ BACKGROUND-HEIGHT 2)
    (place-image/align
      (make-happiness-bar (cat-happiness ws))
      0
      BACKGROUND-HEIGHT
      "left"
      "bottom"
      BACKGROUND)))


; Cat -> Image
; Returns an image that represents cat in the current state
(define (get-cat-image cat)
  (if (= (cat-happiness cat) 0) 
    (rotate 90 cat-image)
    cat-image))


; Cat -> Cat
(define (tock cat)
  (cond 
    [(unhappy? cat) cat]
    [(exiting-right? cat) (make-cat BACKGROUND-WIDTH "left" (cat-happiness cat))]
    [(exiting-left? cat) (make-cat 0 "right" (cat-happiness cat))]
    [else 
      (make-cat
        (if (going-right? cat)
          (+ (cat-pos cat) CAT-SPEED)
          (- (cat-pos cat) CAT-SPEED))
        (cat-direction cat)
        (+ (cat-happiness cat) HAPPINESS-SPEED))]))


; Cat -> Boolean
; Returns whether the cat is completely unhappy
(define (unhappy? cat)
  (<= (cat-happiness cat) 0))


; Cat -> Boolean
; Returns whether the cat is completely happy
(define (fulfilled? cat)
  (>= (cat-happiness cat) 100))


; Cat -> Boolean
; Whether the cat has reached the righmost point of the scenario
(define (exiting-right? cat)
  (> (cat-pos cat) BACKGROUND-WIDTH))


; Cat -> Boolean
; Whether the cat has reached the leftmost point of the scenario
(define (exiting-left? cat)
  (< (cat-pos cat) 0))


; Cat -> Boolean
; Whether the cat is moving to the right direction
(define (going-right? cat)
  (string=? (cat-direction cat) "right"))


; Happiness -> Image
; Given a level of happiness, returns an image representing it
(define (make-happiness-bar h)
  (rectangle 
    (*  BACKGROUND-WIDTH (/ h 100)) 
    10 
    "solid" 
    (if (>= h 20) "olivedrab" "red")))


; Cat KeyEvent -> Cat
(define (on-key-press cat key)
  (if 
    (fulfilled? cat) cat
    (make-cat 
      (cat-pos cat)
      (cat-direction cat)
      (+ 
        (cat-happiness cat)
        (cond
          [(string=? key "up") 5]
          [(string=? key "down") 3]
          [else 0])))))


(define (main cat)
  (big-bang cat
    [to-draw render-world]
    [on-key on-key-press]
    [on-tick tock]))


(main (make-cat 0 "right" 100))
