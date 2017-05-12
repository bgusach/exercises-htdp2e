#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; ### Constants
(define BACKGROUND-WIDTH 300)
(define BACKGROUND-HEIGHT 200)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))


; ### Data Definitions
; A FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a Color.
; Interpretation A FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to key strokes

; WorldState is a structure (make-world ...)
; Interpretation:
(define-struct world [...])


; ==================== Exercise 226 ====================
; ### Functions
; FSM-State FSM-State -> Boolean
; Returns whether the two passed states are equal
(define (state=? s1 s2)
  (cond
    [(not (image-color? s1)) (error (format "'~a' is not a valid state") s1)]
    [(not (image-color? s2)) (error (format "'~a' is not a valid state") s2)]
    [else (string=? s1 s2)]
    ))



; =================== End of exercise ==================



; ==================== Exercise 227 ====================
; ### Constants
(define 
  fsm-wb
  (list
    (make-transition "black" "white")
    (make-transition "white" "black")
    ))

; =================== End of exercise ==================





; WorldState -> Image
(define (render-world ws)
  BACKGROUND
  )


; WorldState KeyEvent -> WorldState
; Handles the key events
(define (on-key-press ws ke)
  (cond
    [(key=? ke "...") ws]
    [else ws]
    ))


; WorldState -> WorldState
; Handles the ticking of the world
(define (tock ws)
  ws
  )


; WorldState -> Boolean
; Predicate to define when the world comes to an end
(define (over? ws)
  #false
  )


; WorldState -> Image
; Renders the last image after the world ended
(define (render-final ws)
  (render-world ws)
  )


(define (main ws)
  (big-bang 
    ws
    [to-draw render-world]
    [on-key on-key-press]
    [on-tick tock]
    [stop-when over? render-final]
    [check-with world?]
    ))


(test)
(main (make-world ...))

