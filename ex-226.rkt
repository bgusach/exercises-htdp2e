#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; ### Constants
(define BACKGROUND-WIDTH 300)
(define BACKGROUND-HEIGHT 200)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))


; ### Data Definitions

; WorldState is a structure (make-world ...)
; Interpretation:
(define-struct world [...])

; A FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a Color.

; interpretation A FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to key strokes 

; ### Functions

; ==================== Exercise 226 ====================

; State State -> Boolean
; Returns whether both states are the same
(check-expect (state=? "blue" "blue") #true)
(check-expect (state=? "blue" "yellow") #false)
(define (state=? a b)
  (string=? a b)
  )



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
; (main (make-world ...))

