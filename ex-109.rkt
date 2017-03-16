#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; CONSTANTS
(define BACKGROUND-WIDTH 300)
(define BACKGROUND-HEIGHT 200)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))


; DATA DEFINITIONS

; Status is one of
; - "accept-a"
; - "accept-b-c-d"
; - "finished"
; - "error"


; FUNCTIONS

; Status -> Image
(define (render-world ws)
  (overlay
    (cond
      [(string=? ws "accept-a") (make-rectangle "whitesmoke")]
      [(string=? ws "accept-b-c-d") (make-rectangle "yellow")]
      [(string=? ws "finished") (make-rectangle "green")]
      [(string=? ws "error") (make-rectangle "red")]
      )
    
    BACKGROUND
    ))

; Colour -> Image
(define (make-rectangle colour)
  (rectangle 100 100 "solid" colour)
  )


; WorldState KeyEvent -> WorldState
; handles the key events
(define (on-key-press ws ke)
  (cond
    [(string=? ws "accept-a") 
     (if 
       (key=? ke "a")
       "accept-b-c-d"
       "error"
       )]

    [(string=? ws "accept-b-c-d")
      (cond
        [(key=? ke "b") ws]
        [(key=? ke "c") ws]
        [(key=? ke "d") "finished"]
        )]

    [(string=? ws "finished") ws]
    [(string=? ws "error") ws]
    ))


(define (main ws)
  (big-bang 
    ws
    [to-draw render-world]
    [on-key on-key-press]
    ))


(main "accept-a")
