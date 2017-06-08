#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; ### Data Definitions

(define-struct world [fsm current])
; A WorldState is a structure
; (make-world FSM FSM-State)

(define-struct fsm [initial transitions final])
; A FSM is a structure
; (make-fsm FSM-State LOT FSM-State)

; A FSM-State is a String
; Intepretation: a given state of a finite state machine

; An LOT is one of:
; - '()
; - (cons Transition LOT)


(define-struct transition [current key next])
; A Transition is a structure
; (make-transition FSM-State KeyEvent FSM-State


(define 
  my-fsm
  (make-fsm
    "white"
    (list
      (make-transition "white" "a" "yellow")
      (make-transition "yellow" "b" "yellow")
      (make-transition "yellow" "c" "yellow")
      (make-transition "yellow" "d" "green")
      )
    "green"
    ))


; ### Functions

; FSM -> World
(define (main fsm)
  (big-bang 
    (make-world fsm (fsm-initial fsm))
    [to-draw render-world]
    [on-key on-key-press]
    [stop-when last-world? render-world]
    ))


; World -> Image
(define (render-world world)
  (square 100 "solid" (world-current world))
  )


; World KeyEvent -> World
(define (on-key-press world ke)
  (make-world
    (world-fsm world)
    (find-next-state 
      (world-current world) 
      (fsm-transitions (world-fsm world))
      ke
      )))


; FSM-State LOT KeyEvent
(define (find-next-state current lot ke)
  (cond
    [(empty? lot) current]

    [(and
       (state=? (transition-current (first lot)) current)
       (key=? (transition-key (first lot)) ke)
       )
     (transition-next (first lot))
     ]

    [else (find-next-state current (rest lot) ke)]
    ))


; FSM-State FSM-State -> Boolean
(define (state=? a b)
  (string=? a b)
  )


; World -> Boolean
(define (last-world? world)
  (state=?
    (fsm-final (world-fsm world))
    (world-current world)
    ))


(main my-fsm)
(test)

