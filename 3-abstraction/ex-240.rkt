#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants

; ### Data Definitions

(define-struct layer [stuff])

; A LStr is one of: 
; – String
; – (make-layer LStr)
; Examples:
"lol"
(make-layer "lol")
(make-layer (make-layer "lol"))
	
; A LNum is one of: 
; – Number
; – (make-layer LNum)
3
(make-layer 3)
(make-layer (make-layer 3))

; A [Layered TYPE] is one of:
; - TYPE
; - (make-layer [Layered TYPE])

; LNum == [Layered Number]
; LStr == [Layered String]

; ### Functions
(test)

