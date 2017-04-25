#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; NOTE: when an exercise modifies a previously existing function,
; a new version name will be used, like *-v2

; ==================== Exercise 215 ====================
; ### Constants
(define TILES-PER-SIDE 30)
(define TILE-WIDTH 10)
(define BACKGROUND-WIDTH (* TILE-WIDTH TILES-PER-SIDE))
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-WIDTH))
(define SNAKE-COLOUR "red")


; ### Data Definitions
; A Direction is one of:
; - "up"
; - "down"
; - "left"
; - "right"
(define DIRECTIONS (list "up" "down" "left" "right"))


; WorldState is a structure (make-world Direction Posn)
; Interpretation: the snake is in pos and moves to direct

; NOTE: The pos is a logical position: how many units instead of
; pixels. This allows for simpler/abstracter thinking, and makes
; things like re-escaling just a breeze.
(define-struct world [pos direct])


; ### Functions
; WorldState -> Image
(define (render-world ws)
  (render-element 
    (world-pos ws)
    SNAKE-COLOUR
    BACKGROUND
    ))


(define (render-element pos col img)
  (underlay/xy
    img
    (* (posn-x pos) TILE-WIDTH)
    (* (posn-y pos) TILE-WIDTH)
    (square TILE-WIDTH "solid" SNAKE-COLOUR)  ; Squares look more retro =)
    ))


; WorldState KeyEvent -> WorldState
; handles the key events
(define (on-key-press ws ke)
  (make-world
    (world-pos ws)
    (cond
      [(member ke DIRECTIONS) ke]
      [else (world-direct ws)]
      )))


; WorldState KeyEvent -> WorldState
; handles the ticking of the world
(define (tock ws)
  (make-world
    (translate-pos (world-pos ws) (world-direct ws))
    (world-direct ws)
    ))


; Posn Direction -> Posn
; Translates the pos one unit according to the direction
(define (translate-pos pos dir)
  (cond
    [(string=? dir "up") (make-posn (posn-x pos) (- (posn-y pos) 1))]
    [(string=? dir "down") (make-posn (posn-x pos) (+ (posn-y pos) 1))]
    [(string=? dir "left") (make-posn (- (posn-x pos) 1) (posn-y pos))]
    [(string=? dir "right") (make-posn (+ (posn-x pos) 1) (posn-y pos))]
    ))


(define (main ws)
  (big-bang 
    ws
    [to-draw render-world]
    [on-key on-key-press]
    [on-tick tock 0.1]
    ))


; (main (make-world (make-posn 0 0) "down"))

; =================== End of exercise ==================




; ==================== Exercise 216 ====================
; suffix for new versions: -v2

; ### Functions
; WorldState -> Boolean
(define (over? ws)
  (hitting-wall? (world-pos ws))
  )


; Posn -> Boolean
; Returns whether the posn is hitting a wall
(define (hitting-wall? pos)
  (or
    (negative? (posn-x pos))
    (negative? (posn-y pos))
    (>= (posn-x pos) TILES-PER-SIDE)
    (>= (posn-y pos) TILES-PER-SIDE)
    ))


; WorldState -> Image
; renders the last image after the world ended
(define (render-final ws)
  (overlay/align
    "left"
    "bottom"
    (text "Bro, you hit the wall!" 24 "black")
    (render-world ws)
    ))


(define (main-v2 ws)
  (big-bang 
    ws
    [to-draw render-world]
    [on-key on-key-press]
    [on-tick tock 0.1]
    [stop-when over? render-final]
    ))


; (main-v2 (make-world (make-posn 0 0) "down"))

; =================== End of exercise ==================




; ==================== Exercise 217 ====================
; suffix for new versions: -v3

; ### Data Definitions
; Trail is one of:
; - (cons Posn '())
; - (cons Posn Trail)
; Interpretation: non empty list of Posns that represent
; all the positions the snake is using


; WorldState is a structure (make-world Direction Trail)
; Interpretation: the snake is placed in trail 
; and moves to direct
(define-struct world-v3 [trail direct])


(define (render-world-v3 ws)
  (render-trail
    (world-v3-trail ws)
    BACKGROUND
    ))


; Trail Image -> Image
(define (render-trail trail img)
  (cond
    [(empty? trail) img]
    [else
      (render-trail 
        (rest trail) 
        (render-element (first trail) SNAKE-COLOUR img)
        )]))


; WorldState KeyEvent -> WorldState
; handles the ticking of the world
(define (tock-v3 ws)
  (make-world-v3
    (move-trail (world-v3-trail ws) (world-v3-direct ws))
    (world-v3-direct ws)
    ))


; Trail Direction -> Trail
; Moves the trail one unit in the passed direction
(check-expect 
  (move-trail (list (make-posn 10 0) (make-posn 9 0)) "down")
  (list (make-posn 10 1) (make-posn 10 0))
  )
(define (move-trail trail direction)
  (cons 
    (translate-pos (first trail) direction)
    (drop-last trail)
    ))


; Non-empty-list-of-anything -> Anything
; Returns the last element of a non empty list
(check-expect (last (list 1 2 3)) 3)
(check-expect (last (list 1)) 1)
(define (last loa)
  (cond
    [(empty? (rest loa)) (first loa)]
    [else (last (rest loa))]
    ))


; Non-empty-list-of-anything -> List-of-anything
; Drops the last element of a non empty list loa
(check-expect (drop-last (list 1 2 3)) (list 1 2))
(define (drop-last loa)
  (cond
    [(empty? (rest loa)) '()]
    [else
      (cons
        (first loa)
        (drop-last (rest loa))
        )]))


; WorldState KeyEvent -> WorldState
; handles the key events
(define (on-key-press-v3 ws ke)
  (make-world-v3
    (world-v3-trail ws)
    (cond
      [(member ke DIRECTIONS) ke]
      [else (world-v3-direct ws)]
      )))


(define (main-v3 ws)
  (big-bang 
    ws
    [to-draw render-world-v3]
    [on-key on-key-press-v3]
    [on-tick tock-v3 0.1]
    ))

; (main-v3 (make-world-v3 (list (make-posn 2 0) (make-posn 1 0) (make-posn 0 0)) "down"))

; =================== End of exercise ==================




; ==================== Exercise 218 ====================
; suffix for new versions: -v4

; ### Constants
(define running "running")
(define hit-wall "hit-wall")
(define hit-itself "hit-itself")

; ### Data Definitions
; GameStatus is one of:
; - running
; - hit-wall
; - hit-itself


; WorldState is a structure (make-world Direction Trail GameStatus)
; Interpretation: the snake is placed in trail 
; and moves to direct
(define-struct world-v4 [trail direct status])


; WorldState -> Boolean
(define (over? ws)
  (not (string?= (world-v4-status ws)))
  )


; WorldState KeyEvent -> WorldState
; Handles the key events
(define (on-key-press-v4 ws ke)
  (make-world
    (world-v4-trail ws)
    (cond
      [(member ke DIRECTIONS) ke]
      [else (world-v4-direct ws)]
      )
    (world-v4-status ws)
    ))


; WorldState KeyEvent -> WorldState
; Handles the ticking of the world
(define (tock-v4 ws)
  (cond
    [(hitting-wall?) (...hit-wall)]
    [(hitting-itself?) (...hit-wall)]
    [else
      (make-world-v4
        (move-trail (world-v4-trail ws) (world-v4-direct ws))
        (world-v3-direct ws)
        ))
      ]
    )


(define (main-v4 ws)
  (big-bang 
    ws
    [to-draw render-world-v4]
    [on-key on-key-press-v4]
    [on-tick tock-v4 0.1]
    ))

; (main-v3 (make-world-v3 (list (make-posn 2 0) (make-posn 1 0) (make-posn 0 0)) "down"))

; =================== End of exercise ==================


(test)
