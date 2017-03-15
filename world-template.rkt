#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; CONSTANTS
(define BACKGROUND-WIDTH 300)
(define BACKGROUND-HEIGHT 200)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))


; DATA DEFINITIONS

; WorldState is a structure (world ...)
; interpretation:
(define-struct world [...])


; FUNCTIONS

; WorldState -> Image
(define (render-world ws)
  BACKGROUND)


; WorldState KeyEvent -> WorldState
; handles the key events
(define (on-key ws ke)
  (cond
    [(key? ke "...") ...])
    [else ws])


; WorldState KeyEvent -> WorldState
; handles the ticking of the world
(define (tock ws)
  ws)


; WorldState -> Boolean
; predicate to define when the world comes to an end
(define (over? ws)
  #false)


; WorldState -> Image
; renders the last image after the world ended
(define (render-final ws)
  (render-world ws))


(define (main ws)
  (big-bang 
    ws
    [to-draw render-world]
    [on-key on-key-press]
    [on-tick tock]
    [stop-when over? render-final]))


; TEST & MAIN CALL
(test)
(main (world ...))
