#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)


(define cham-image (bitmap "../images/cham.png"))
(define BACKGROUND-WIDTH 300)
(define BACKGROUND-HEIGHT 180)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))
(define SPEED 3)
(define HAPPINESS-SPEED -1)


; A ChamColour is one of 
; - "red"
; - "blue"
; - "green"
; interpretation: colour of the chameleon

; A Position is a Number in [0, 100]
; interpretation: 0 leftmost position 100 rightmost position

; A Happiness is a Number in [0, 100]
; interpretation: 0 minimum happiness, 100 maximum


(define-struct cham [pos colour happiness])
; VCham is (make-cham Position ChamColour Happiness)
; intepretation: state of the world defined by
; where is the chameleon, what colour it has and how happy it is


; VCham -> Image
(define (render-world ws)
  (place-image
    (get-cham-image ws)
    (cham-pos ws)
    (/ BACKGROUND-HEIGHT 2)
    (place-image/align
      (make-happiness-bar (cham-happiness ws))
      0
      BACKGROUND-HEIGHT
      "left"
      "bottom"
      BACKGROUND)))


; VCham -> Image
; Returns an image that represents cham in the current state
(define (get-cham-image cham)
  (overlay
    (if (= (cham-happiness cham) 0) 
      (rotate 90 cham-image)
      cham-image)

    (rectangle 
      (image-width cham-image) 
      (image-height cham-image) 
      "solid" 
      (cham-colour cham))))


; VCham -> VCham
(define (tock cham)
  (cond 
    [(unhappy? cham) cham]
    [else 
      (make-cham
        (modulo (+ (cham-pos cham) SPEED) BACKGROUND-WIDTH)
        (cham-colour cham)
        (+ (cham-happiness cham) HAPPINESS-SPEED))]))


; VCham -> Boolean
; Returns whether the cham is completely unhappy
(define (unhappy? cham)
  (<= (cham-happiness cham) 0))


; VCham -> Boolean
; Returns whether the cham is completely happy
(define (fulfilled? cham)
  (>= (cham-happiness cham) 100))


; Happiness -> Image
; Given a level of happiness, returns an image representing it
(define (make-happiness-bar h)
  (rectangle 
    (*  BACKGROUND-WIDTH (/ h 100)) 
    10 
    "solid" 
    (if (>= h 20) "olivedrab" "red")))


; VCham KeyEvent -> VCham
(define (on-key-press cham key)
  (cond
    [(and (key=? key "down") (not (fulfilled? cham)))
     (make-cham 
       (cham-pos cham) 
       (cham-colour cham)
       (+ (cham-happiness cham) 2))]

    [(key=? key "r")
     (make-cham 
       (cham-pos cham) 
       "red"
       (cham-happiness cham))]

    [(key=? key "b")
     (make-cham 
       (cham-pos cham) 
       "blue"
       (cham-happiness cham))]

    [(key=? key "g")
     (make-cham 
       (cham-pos cham) 
       "green"
       (cham-happiness cham))]

    [else cham]))

(define (main cham)
  (big-bang cham
    [to-draw render-world]
    [on-key on-key-press]
    [on-tick tock]))


(main (make-cham 0 "green" 100))
