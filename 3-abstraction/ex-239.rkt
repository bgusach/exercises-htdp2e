#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants

; ### Data Definitions

; A [List X Y] is a structure:
;   (cons X (cons Y '()))

; A [List Number Number] is a structure:
;   (cons Number (cons Number '()))
; For instance:
(list 42 42)

; A [List Number 1String] is a structure:
;   (cons Number (cons 1String '()))
; For instance:
(list 42 "a")

; A [List String Boolean] is a structure:
;   (cons String (cons Boolean '()))
; For instance:
(list "hallo" #false)

; ### Functions
(test)

