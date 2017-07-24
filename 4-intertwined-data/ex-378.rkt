#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)


; ### Data definitions

; A FSM is a [List-of 1Transition]

; A 1Transition is a list of two items:
;   (cons FSM-State (cons FSM-State '()))

; A FSM-State is a String that specifies a color
 
; Example:
(define fsm-traffic
  '(("red" "green")
    ("green" "yellow")
    ("yellow" "red")
    ))


; ### Functions


; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local 
    ((define fm (assoc x alist))
     )

    ; -- IN --
    (if 
      (cons? fm) 
      (second fm) 
      (error "not found")
      )))

; ==================== Exercise 378 ====================

; FSM FSM-State -> FSM-State 
; match the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw render-state]
    [on-key 
      (λ (current key-event) (find transitions current))
      ]))


; FSM-State -> Image
; Renders the current state
(define (render-state state)
  (overlay
    (text state 30 "grey")
    (square 100 "solid" state)
    ))

; (simulate "red" fsm-traffic)

; =================== End of exercise ==================




; ==================== Exercise 379 ====================

(check-expect (find '(("a" "b") ("c" "d")) "c") "d")
(check-error (find '(("a" "b") ("c" "d")) "z"))

; =================== End of exercise ==================




; ==================== Exercise 380 ====================

; A StateKey is a list of two items:
;   (cons FSM-State (const KeyEvent '()))
; Example:
; '("red" "g")

; A 1Transition.v2 is a list of two items:
;   (cons StateKeys (cons FSM-State '()))
; 
; Example:
;  '(("red" "g") "green")

; An FSM.v2 is a [List-of 1Transition.v2]
; Example:
(define fsm-traffic.v2
  '((("red" "g") "green")
    (("green" "y") "yellow")
    (("yellow" "r") "red")
    ))

(check-expect (find fsm-traffic.v2 '("red" "g")) "green")


; FSM FSM-State -> FSM-State 
(define (simulate.v2 state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw render-state]
    [on-key 
      (λ (current key-event) 
         (find transitions (list current key-event))
         )]))


; (simulate.v2 "red" fsm-traffic.v2)

; =================== End of exercise ==================

; An XMachine is a nested list of this shape:
; `(machine ((initial ,FSM-State)) [List-of X1T])

; An X1T is a nested list of this shape:
; `(action ((state ,FSM-State) (next ,FSM-State)))



; ==================== Exercise 381 ====================

; An XMachine is a nested list of this shape:
; `(machine ((initial ,FSM-State)) [List-of X1T])
; or equivalently:
; (list 'machine (list (list 'initial FSM-State)) [List-of X1T])

; An X1T is a nested list of this shape:
; (list 'action 
;   (list 
;     (list 'state FSM-State)
;     (list 'next  FSM-State)
;     ))

; NOTE: I think with cons is just unintelligible

; =================== End of exercise ==================




; ==================== Exercise 382 ====================

; <machine initial="black">
;   <action state="black" next="white" />
;   <action state="white" next="black" />
; </machine>

(define b&w-fsm
  '(machine 
     ((initial "black"))
     (action ((state "black") (next "white")))
     (action ((state "white") (next "black")))
     ))

; =================== End of exercise ==================

; NOTE: next funcs directly copied from previous exercises
; [List-of Attribute] Symbol -> [Maybe String]
(define (find-attr attrs name)
  (local
    ((define res (assq name attrs)))

    ; -- IN --
    (if 
      (false? res) 
      #false
      (second res)
      )))


; Xexpr.v2 -> [List-of Attribute]
; Retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local
    ((define attr+children (rest xe)))

    ; -- IN --
    (match
      attr+children
      ['() '()]
      [(cons head _)
       (if (list-of-attributes? head) head '())
       ]
      [else '()]
      )))


; XMachine -> [List-of 1Transition]
; extracts the transition table from xm
(check-expect (xm->transitions xm0) fsm-traffic)
(define (xm->transitions xm)
  (local 
    (; X1T -> 1Transition
     (define (xaction->action xa)
       (list 
         (find-attr (xexpr-attr xa) 'state)
         (find-attr (xexpr-attr xa) 'next)
         )))

    ; -- IN --
    (map xaction->action (xexpr-content xm))
    ))

; [List-of Attribute] or Xexpr.v2 -> Boolean
; Returns whether the given value is a list of attributes
(define (list-of-attributes? x)
  (match
    x
    ['() #true]
    [(cons (? cons?) _) #true]
    [else #false]
    ))


; Xexpr.v2 -> XEConts
; Returns the contents (i.e. children) of `xe`
(define (xexpr-content xe)
  (local
    ((define _ (xexpr-name xe))  ; Just to crash gracefully
     (define loa+content (rest xe))
     )

     ; -- IN --
     (match
       loa+content
       ['() '()]
       [(cons head tail)
        (if 
          (list-of-attributes? head)
          tail
          loa+content
          )])))


; Xexpr.v2 -> Symbol
; Returns the name of `xe`
(define (xexpr-name xe)
  (match
    xe
    [(cons (? symbol?) _) (first xe)]
    [else (error (format "Expected Xexpr, got ~s instead" xe))]
    ))


; XMachine -> FSM-State
; simulates an FSM via the given configuration 
(define (simulate-xmachine xm)
  (simulate 
    (xm-state0 xm)
    (xm->transitions xm)
    ))


(define xm0
  '(machine ((initial "red"))
     (action ((state "red") (next "green")))
     (action ((state "green") (next "yellow")))
     (action ((state "yellow") (next "red")))
     ))


; XMachine -> FSM-State
(check-expect (xm-state0 xm0) "red")
(define (xm-state0 xm0)
  (find-attr (xexpr-attr xm0) 'initial)
  )

(simulate-xmachine xm0)

(test)

