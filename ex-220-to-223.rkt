#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; ### Constants
(define HEIGHT 20)
(define WIDTH 10)
(define UNIT-WIDTH 40)
(define BACKGROUND (empty-scene (* WIDTH UNIT-WIDTH) (* HEIGHT UNIT-WIDTH)))
(define BLOCK
  (overlay
    (square (sub1 UNIT-WIDTH) "solid" "red")
    (square UNIT-WIDTH "outline" "black")
    ))


; ### Data Definitions

(define-struct block [x y])
; Block is a data structure (make-block x y)
; where x and y are coordinates
; 


; A Landscape is one of:
; - '()
; - (cons Block Landscape)

; A Tetris is a structure: (make-tetris Block Landscape)
; interpretation: Block is the falling block, Landscape
; already placed blocks
(define-struct tetris [block landscape])


; ==================== Exercise 220 ====================
(define block0 (make-block 0 0))
(define block1 (make-block 0 (sub1 HEIGHT)))
(define block2 (make-block (sub1 WIDTH) (sub1 HEIGHT)))
(define landscape0 (list block1 block2))

; Tetris -> Image
(define (render-tetris t)
    (render-block 
      (tetris-block t)
      (render-landscape (tetris-landscape t) BACKGROUND)
      ))


; Block Image -> Image
; Renders the block on top of the image
(define (render-block b img)
  (underlay/xy
    img
    (* (block-x b) UNIT-WIDTH)
    (* (block-y b) UNIT-WIDTH)
    BLOCK
    ))


; Landscape Image -> Image
(define (render-landscape l img)
  (cond
    [(empty? l) img]
    [else
      (render-block 
        (first l)
        (render-landscape (rest l) img)
        )]))


; =================== End of exercise ==================




; ==================== Exercise 221 ====================
; ### Constants
(define d-left "left")
(define d-right "right")
(define d-down "down")

; ### Data Definitions
; Direction is one of:
; - d-left
; - d-right
; - d-down


; ### Functions

; Tetris -> Tetris
; Handles the ticking of the world
(define (tock t)
  (tock-helper 
    (tetris-block t)
    (translate-block (tetris-block t) d-down)
    (tetris-landscape t)
    ))


; Tick handler where all variables have already been resolved 
; to avoid annoying code duplication
(define (tock-helper this-block next-block landscape)
  (if
    (block-hitting? next-block landscape)

    (make-tetris
      (make-block 
        (choose-another-column (block-x this-block))
         0
         )
       (cons this-block landscape)
       )

    (make-tetris
      next-block
      landscape
      )))


; Number -> Number
; Returns a different column than the previous
(define (choose-another-column c)
  (choose-another-column-helper c (random WIDTH))
  )


; Function that complements choose-another-column
(define (choose-another-column-helper pre new)
  (if
    (= pre new)
    (choose-another-column pre)
    new
    ))


; Block Landscape -> Boolean
; Returns whether the block is hitting anything (floor or other block)
(define (block-hitting? b landscape)
  (or
    (= (block-y b) HEIGHT)
    (member b landscape)
    ))


; Block Direction -> Block
(check-expect (translate-block (make-block 0 0) d-down) (make-block 0 1))
(check-expect (translate-block (make-block 0 0) d-right) (make-block 1 0))
(check-expect (translate-block (make-block 1 0) d-left) (make-block 0 0))
(define (translate-block b dir)
  (cond
    [(string=? dir d-down) 
     (make-block 
       (block-x b)
       (add1 (block-y b))
       )]

    [(string=? dir d-right) 
     (make-block 
       (add1 (block-x b))
       (block-y b)
       )]

    [(string=? dir d-left) 
     (make-block 
       (sub1 (block-x b))
       (block-y b)
       )]

    [else (error (format "Unexpected direction: ~a" dir))]
    ))



(define (main ws rate)
  (big-bang 
    ws
    [to-draw render-tetris]
    [on-tick tock rate]
    ))

; (main (make-tetris block0 '()) 0.1)

; =================== End of exercise ==================




; ==================== Exercise 222 ====================
; Suffix for new versions: -v2

; ### Function definitions

; Tetris KeyEvent -> WorldState
; Handles the key events
(define (on-key-press tetris ke)
  (if
    (member ke (list d-left d-right))

    (evaluate-movement (move-block (tetris-block tetris) ke) tetris)
    tetris
    ))


; Block Direction -> Block
; Moves the block b to the passed direction
(define (move-block b direction)
  (cond
    [(string=? direction d-left) (make-block (sub1 (block-x b)) (block-y b))]
    [(string=? direction d-right) (make-block (add1 (block-x b)) (block-y b))]
    [(string=? direction d-down) (make-block (block-x b) (add1 (block-y b)))]
    [else (error (format "Illegal direction: ~a" direction))]
    ))


; Block Tetris -> Tetris
; Given the candidate for next block and current tetris
; state, it returns the new tetris state
(define (evaluate-movement block-candidate tetris)
  (cond
    [(or
       (< (block-x block-candidate) 0)
       (> (block-x block-candidate) (sub1 WIDTH))
       (member block-candidate (tetris-landscape tetris))
       )

     tetris
     ]

    [else 
     (make-tetris
       block-candidate
       (tetris-landscape tetris)
       )]))


(define (main-v2 ws rate)
  (big-bang 
    ws
    [to-draw render-tetris]
    [on-key on-key-press]
    [on-tick tock rate]
    ; [stop-when over? render-final]
    ))


(main-v2 (make-tetris block0 '()) 0.1)

; =================== End of exercise ==================

; WorldState -> Boolean
; Predicate to define when the world comes to an end
(define (over? ws)
  #false
  )


; WorldState -> Image
; Renders the last image after the world ended
; (define (render-final ws)
;   (render-world ws)
;   )


(test)

