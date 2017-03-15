#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

(define BACKGROUND-WIDTH 300)
(define BACKGROUND-HEIGHT 180)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))
(define SPEED 3)
(define HAPPINESS-SPEED -1)
(define HAPPINESS-FOOD 2)
(define HAPPINESS-PET 5)
(define MIN-HAPP 0)
(define MAX-HAPP 100)


; Happiness is a Number in [0, 100]
; interpretation: 0 absolute misery, 100 utter bliss


; Position is a Number
; interpretation: position in the x-coordinate


; Direction is one of
; - "left"
; - "right"


; An VCat is a structure (make-cat Position Direction Happiness)
; interpretation: x is the Position of the cat, d its Direction and h its Happiness
(define-struct cat [x d h])

(define cat-image (bitmap "images/cat.png"))

; Colour is one of
; - "r"
; - "b"
; - "g"


; VCham is a structure (make-cham Position Colour Happiness)
; intepretation: state of the world defined by
; where is the chameleon, what colour it has and how happy it is
(define-struct cham [x c h])


(define cham-image (bitmap "images/cham.png"))

; A VAnimal is one of
; - a VCat
; - a VCham


; VAnimal -> Image
(define (render-world a)
  (cond
    [(cat? a) (render-cat a BACKGROUND)]
    [(cham? a) (render-cham a BACKGROUND)]
    ))


; VCat Image -> Image
(define (render-cat c img)
  (place-image
    cat-image
    (cat-x c)
    (/ BACKGROUND-HEIGHT 2)
    (make-happiness-bar (cat-h c) img)
    ))


; VCham Image -> Image
(define (render-cham c img)
  (place-image
    cham-image
    (cham-x c)
    (/ BACKGROUND-HEIGHT 2)
    (place-image
      (rectangle 
        (image-width cham-image) 
        (image-height cham-image) 
        "solid" 
        (normalize-colour (cham-c c))
        )
      (cham-x c)
      (/ BACKGROUND-HEIGHT 2)
      (make-happiness-bar (cham-h c) img)
      )))


; Colour -> image-colour?
; converts an item of the Colour enumeration into a image-colour?
(define (normalize-colour c)
  (cond
    [(string=? c "r") "red"]
    [(string=? c "g") "green"]
    [(string=? c "b") "blue"]
    ))


; VAnimal -> VAnimal
(define (tock a)
  (cond
    [(cat? a) (tock-cat a)]
    [(cham? a) (tock-cham a)]
    ))


(define (tock-cat c)
  (make-cat 
    (if 
      (> (cat-h c) 0)
      (cond
        [(string=? (cat-d c) "right") (+ (cat-x c) SPEED)]
        [(string=? (cat-d c) "left") (- (cat-x c) SPEED)]
        )
      (cat-x c)
      )
    (cond 
      [(<= (cat-x c) 0) "right"]
      [(>= (cat-x c) BACKGROUND-WIDTH) "left"]
      [else (cat-d c)]
      )
    (happiness-sum (cat-h c) HAPPINESS-SPEED)
    ))


(define (tock-cham c)
  (make-cham 
    (modulo 
      (+ 
        (cham-x c) 
        (if (> (cham-h c) 0) SPEED 0)
        )
      BACKGROUND-WIDTH)
    (cham-c c)
    (happiness-sum (cham-h c) HAPPINESS-SPEED)
    ))


(define (happiness-sum a b)
  (cap-num
    MIN-HAPP
    MAX-HAPP
    (+ a b)
    ))

; VAnimal -> VAnimal
(define (key-handler a ke)
  (cond
    [(cat? a) (cat-key-handler a ke)]
    [(cham? a) (cham-key-handler a ke)]
    ))


(define (cat-key-handler c ke)
  (make-cat
    (cat-x c)
    (cat-d c)
    (+
      (cat-h c)
      (cond
        [(key=? ke "down") HAPPINESS-FOOD]
        [(key=? ke "up") HAPPINESS-PET]
        [else 0]
        ))))


(define (cham-key-handler c ke)
  (cond
    [(key=? ke "down") 
     (make-cham
       (cham-x c)
       (cham-c c)
       (happiness-sum (cham-h c) HAPPINESS-FOOD)
       )]

    [(or (key=? ke "r") (key=? ke "b") (key=? ke "g"))
     (make-cham (cham-x c) ke (cham-h c))]
    
    [else c]))


; Number Number Number -> Number
; given a min, a max and a number, it returns the number capped between
; the min and the max
(check-expect (cap-num 0 10 10) 10)
(check-expect (cap-num 0 10 12) 10)
(check-expect (cap-num 0 10 -4) 0)
(check-expect (cap-num 0 10 3) 3)
(define (cap-num min max num)
  (cond
    [(< num min) min]
    [(> num max) max]
    [else num]
    ))


; Happiness Image -> Image
; Given a level of happiness and an image, renders a gauge on that image
(define (make-happiness-bar h img)
  (place-image/align
    (rectangle 
      (*  (image-width img) (/ h 100)) 
      10 
      "solid" 
      (if (>= h 20) "olivedrab" "red")
      )
    0
    BACKGROUND-HEIGHT
    "left"
    "bottom"
    img
    ))


(define (main a)
  (big-bang 
    a
    [to-draw render-world]
    [on-key key-handler]
    [on-tick tock]
    ))


(test)
(main (make-cham 0 "g" 100))
(main (make-cat 100 "right" 100))
