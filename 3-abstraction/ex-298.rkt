#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; ### Constants
(define BACKGROUND-WIDTH 300)
(define BACKGROUND-HEIGHT 200)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))
(define ROCKET (bitmap "../images/rocket.png"))   


; ### Data Definitions

; An ImageStream is a function
; [N -> Image]

; ImageStream
(define (create-rocket-scene height)
  (place-image ROCKET 70 height BACKGROUND)
  )


; ### Functions

; [N -> Image] N -> N
; Renders the stream for `ticks` at a rate of 1/30
(define (my-animate stream ticks)
  (big-bang 
    0
    [to-draw stream]
    [on-tick add1 1/30]
    [stop-when (λ (n) (= n ticks))]
    ))

(my-animate create-rocket-scene 50)

(test)

