#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; ### CONSTANTS
(define HEIGHT 400)
(define WIDTH 600)
(define XSHOTS (/ WIDTH 2))
(define YSPEED -5)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 5 "solid" "red"))


; ### DATA DEFINITIONS

; A Shot is a integer Number
; interpretation: 10 is a shot at 10px from top

; A List-of-shots is one of:
; - '()
; - (cons Shot List-of-shots)
; 
; interpretation: list of fired shots

; A WorldState is a List-of-shots


; ### FUNCTIONS

; WorldState -> Image
(define (render-world ws)
  (cond
    [(empty? ws) BACKGROUND]
    [else
      (place-image
        SHOT
        XSHOTS
        (first ws) 
        (render-world (rest ws))
        )]))


; WorldState KeyEvent -> WorldState
; handles the key events
(check-expect (on-key-press '() " ") (cons HEIGHT '()))
(check-expect (on-key-press (cons 50 '()) " ") (cons HEIGHT (cons 50 '())))
(define (on-key-press ws ke)
  (cond
    [(key=? ke " ") (cons HEIGHT ws)]
    [else ws]
    ))


; WorldState KeyEvent -> WorldState
; handles the ticking of the world
(check-expect (move-shots '()) '())
(check-expect (move-shots (cons 10 '())) (cons (+ 10 YSPEED) '()))

(define (move-shots ws)
  (cond
    [(empty? ws) '()]
    [else 
      (cons
        (+ (first ws) YSPEED)
        (move-shots (rest ws))
        )])) 


(define (main ws)
  (big-bang 
    ws
    [to-draw render-world]
    [on-key on-key-press]
    [on-tick move-shots]
    ))


(test)
(main '())
