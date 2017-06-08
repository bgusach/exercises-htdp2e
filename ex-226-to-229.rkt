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

; interpretation A FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to key strokes 


(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))


; ==================== Exercise 226 ====================
; ### Functions


; State State -> Boolean
; Returns whether both states are the same
(check-expect (state=? "blue" "blue") #true)
(check-expect (state=? "blue" "yellow") #false)
(define (state=? a b)
  (string=? a b)
  )

; =================== End of exercise ==================



; ==================== Exercise 227 ====================
(define bw-traffic
  (list
    (make-transition "black" "white")
    (make-transition "white" "black")
    ))

; =================== End of exercise ==================



(define-struct fs [fsm current])
; SimulationState.v2 is a structure: (make-fs FSM FSM-State)


; SimulationState.v2 KeyEvent -> SimulationState.v2
; Handles the key events
(check-expect
  (on-key-press (make-fs fsm-traffic "red") "n")
  (make-fs fsm-traffic "green"))
(check-expect
  (on-key-press (make-fs fsm-traffic "red") "a")
  (make-fs fsm-traffic "green"))
(check-expect
  (on-key-press (make-fs fsm-traffic "green") "q")
  (make-fs fsm-traffic "yellow"))
(define (on-key-press world ke)
  (make-fs
    (fs-fsm world)
    (find-next-state (fs-fsm world) (fs-current world))
    ))


; ==================== Exercise 228 ====================

; FSM FSM-State -> FSM-State
; Returns the next state of the FSM
(check-expect (find-next-state fsm-traffic "green") "yellow")
(define (find-next-state fsm current)
  (cond
    [(empty? fsm) (error "Could not find next state")]
    [(state=? 
       (transition-current (first fsm)) 
       current
       )
     (transition-next (first fsm))
     ]
    [else (find-next-state (rest fsm) current)]
    ))

; =================== End of exercise ==================


; SimulationState.v2 -> Image
(define (render-world world)
  (square 100 "solid" (fs-current world))
  )

; FSM FSM-State -> SimulationState.v2
(define (main a-fsm state-0)
  (big-bang 
    (make-fs a-fsm state-0)
    [to-draw render-world]
    [on-key on-key-press]
    ))

; (main fsm-traffic "red")
; (main bw-traffic "black")


; ==================== Exercise 229 ====================
; ### Data Definitions

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

; A FSM.v2 is one of:
; - '()
; - (cond Transition.v2 FSM.v2)


(define ex-109-fsm 
  (list
    (make-ktransition "white" "a" "yellow")
    (make-ktransition "yellow" "b" "yellow")
    (make-ktransition "yellow" "c" "yellow")
    (make-ktransition "yellow" "d" "green")
    ))


; SimulationState.v3 is a structure: (make-fs FSM.v2 FSM-State)


; FSM.v2 FSM-State KeyEvent -> FSM-State
(define (find-next-state.v2 fsm current ke)
  (cond
    [(empty? fsm) current]
    [(and
       (state=?  (ktransition-current (first fsm)) current)
       (key=? (ktransition-key (first fsm)) ke)
       )

       (ktransition-next (first fsm))
     ]
    [else (find-next-state.v2 (rest fsm) current ke)]
    ))


; SimulationState.v3 KeyEvent -> SimulationState.v3
(define (on-key-press.v2 ws ke)
  (make-fs
    (fs-fsm ws)
    (find-next-state.v2 (fs-fsm ws) (fs-current ws) ke)
    ))


; =================== End of exercise ==================


; FSM FSM-State -> SimulationState.v2
(define (main.v2 a-fsm state-0)
  (big-bang 
    (make-fs a-fsm state-0)
    [to-draw render-world]
    [on-key on-key-press.v2]
    ))

(main.v2 ex-109-fsm "white")
(test)

