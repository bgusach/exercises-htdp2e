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


(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)


(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))


; ### Functions

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
(check-expect 
  (state-as-colored-square (make-fs fsm-traffic "red"))
  (square 100 "solid" "red")
  )
(define (state-as-colored-square a-fs)
    (square 100 "solid" (fs-current a-fs)))


; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "n")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "a")
  (make-fs fsm-traffic "green"))
(define (find-next-state a-fs ke)
  (make-fs
    (fs-fsm a-fs)
    (find (fs-fsm a-fs) (fs-current a-fs))
    ))


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




; ==================== Exercise 228 ====================

; ### Functions

; FSM FSM-State -> FSM-State
; Finds the state representing current in transitions
; and retrieve the next field 
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-error (find fsm-traffic "black") "not found: black")
(check-expect (find fsm-wb "black") "white")
(check-expect (find fsm-wb "white") "black")
(define (find transitions current)
  (cond
    [(empty? transitions) (error (format "not found: ~a" current))]
    [(state=? (transition-current (first transitions)) current)
     (transition-next (first transitions))      
     ]
    [else (find (rest transitions) current)]
    ))


(define (simulate fsm fsm-state)
  (big-bang 
    (make-fs fsm fsm-state)
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    ))

; (simulate fsm-wb "black")
; (simulate fsm-traffic "red")
; =================== End of exercise ==================




; ==================== Exercise 228 ====================
; ### Data Definitions

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State))



(define (simulate.v3 fsm fsm-state)
  (big-bang 
    (make-fs fsm fsm-state)
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    ))


; =================== End of exercise ==================



(test)
(simulate fsm-wb "black")
(simulate fsm-traffic "red")

