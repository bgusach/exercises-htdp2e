#lang htdp/isl

(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)   
(require test-engine/racket-tests)


; ### Constants
(define X-BLOCKS 30)
(define Y-BLOCKS 20)
(define BLOCK-WIDTH 10)
(define BACKGROUND-WIDTH (* BLOCK-WIDTH X-BLOCKS))
(define BACKGROUND-HEIGHT (* BLOCK-WIDTH Y-BLOCKS))
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))
(define SNAKE-COLOUR "red")
(define FOOD-COLOUR "green")

(define running "running")
(define hit-wall "hit-wall")
(define hit-itself "hit-itself")

(define d-up "up")
(define d-down "down")
(define d-left "left")
(define d-right "right")

; ### Data Definitions
; A Direction is one of:
; - d-up
; - d-down
; - d-left
; - d-right
(define DIRECTIONS (list d-up d-down d-left d-right))


; ### Data Definition

; Food is a Posn
; Interpretation: the position where the food is at given moment

; DirectionRequest is one of:
; - Direction
; - #false
; Interpretation: direction requested for next world tick

; WorldState is a structure:
;   (make-world Trail Direction DirectionRequest GameStatus Food)
; 
; Interpretation: 
; - trail: the space the snake is taking. First element is the head
; - direct: direction the snake is currently moving
; - req-dir: requested direction for next tick
; - status: status of game
; - food: position of food
(define-struct world [trail direct req-dir status food])


; ### Functions

; WorldState KeyEvent -> WorldState
; Handles the ticking of the world
(define (tock ws)
  (local
    ((define trail (world-trail ws))
     (define head (first trail))
     (define direction (world-direct ws))
     (define 
       next-direction
       (calculate-new-direction (world-direct ws) (world-req-dir ws))
       )
     (define status (world-status ws))
     (define food (world-food ws))
     )

    ; -- IN --
    (cond
      [(hitting-wall? head next-direction) (set-world-status ws hit-wall)]
      [(hitting-itself? trail next-direction) (set-world-status ws hit-itself)]
      [(hitting-food?  trail next-direction food)
       (make-world
          (cons (translate-pos head next-direction) trail)
          direction
          next-direction
          status
          (create-food (translate-pos head direction))
          )]

      [else
        (make-world
          (move-trail trail next-direction)
          next-direction
          #false
          status
          food
          )])))


; Posn Direction -> Posn
; Translates the pos one unit according to the direction
(define (translate-pos pos dir)
  (cond
    [(string=? dir d-up) (make-posn (posn-x pos) (- (posn-y pos) 1))]
    [(string=? dir d-down) (make-posn (posn-x pos) (+ (posn-y pos) 1))]
    [(string=? dir d-left) (make-posn (- (posn-x pos) 1) (posn-y pos))]
    [(string=? dir d-right) (make-posn (+ (posn-x pos) 1) (posn-y pos))]
    ))


; Posn -> Posn
; Returns the new position where the food can be placed, but not in not-here
(define (create-food not-here)
  (check-create-food 
    (make-posn (random X-BLOCKS) (random Y-BLOCKS))
    not-here 
    ))


; Posn Posn -> Posn
; If candidate and not-here do not collide, food is returned, otherwise
; another candidate is requested
(define (check-create-food candidate not-here)
  (if
    (equal? candidate not-here)
    (create-food not-here)
    candidate
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


; Posn Direction -> Boolean
; Returns whether the posn is hitting a wall on the next tick
(define (hitting-wall? pos dir)
  (or
    (and 
      (zero? (posn-x pos)) 
      (string=? dir d-left)
      )
    (and 
      (= (posn-x pos) (sub1 X-BLOCKS))
      (string=? dir d-right)
      )
    (and 
      (zero? (posn-y pos))
      (string=? dir d-up)
      )
    (and 
      (= (posn-y pos) (sub1 Y-BLOCKS))
      (string=? dir d-down)
      )))


; Trail Direction -> Boolean
; Returns whether the trail is hitting itself on the next tick
(define (hitting-itself? trail direction)
  (member 
    (translate-pos (first trail) direction)
    (drop-last (rest trail))
    ))


; Trail Food -> Boolean
; Returns whether the snake is hitting the food with the head in the next tick
(define (hitting-food? trail direction food)
  (equal?
    (translate-pos (first trail) direction)
    food
    ))



; WorldState -> WorldState
; Creates a copy of ws setting a new status
(define (set-world-status ws status)
  (make-world
    (world-trail ws)
    (world-direct ws)
    (world-req-dir ws)
    status
    (world-food ws)
    ))


; Direction DirectionRequest -> Direction
; Calculates the new direction from current and request 
(check-expect (calculate-new-direction d-down d-down) d-down)
(check-expect (calculate-new-direction d-left d-down) d-down)
(check-expect (calculate-new-direction d-up d-down) d-up)
(check-expect (calculate-new-direction d-up #false) d-up)
(define (calculate-new-direction current-dir dir-req)
  (cond
    [(false? dir-req) current-dir]
    [(and 
       (string=? dir-req d-up) 
       (not (string=? current-dir d-down))) 
     d-up
     ]
    [(and 
       (string=? dir-req d-down) 
       (not (string=? current-dir d-up))) 
     d-down
     ]
    [(and 
       (string=? dir-req d-left) 
       (not (string=? current-dir d-right))) 
     d-left
     ]
    [(and 
       (string=? dir-req d-right) 
       (not (string=? current-dir d-left))) 
     d-right
     ]
    [else current-dir]
    ))


; WorldState -> Image
(define (render-world ws)
  (render-element 
    (world-food ws) 
    FOOD-COLOUR 
    (render-trail
      (world-trail ws)
      (above/align
        d-right
        BACKGROUND
        (make-score (calculate-points ws))
        ))))


; WorldState -> Image
; renders the last image after the world ended
(define (render-final ws)
  (overlay
    (make-message
      (cond
        [(string=? (world-status ws) hit-wall) "Ha-ha you hit the wall!"]
        [(string=? (world-status ws) hit-itself) "Ha-ha you hit yourself!"]
        ))
    (render-world ws)
    ))


; WorldState KeyEvent -> WorldState
; Handles the key events
(define (on-key-press ws ke)
  (make-world
    (world-trail ws)
    (world-direct ws)

    ; NOTE: if the direction is changed directly here, weird bugs happen
    ; because sometimes you hit the keys so fast that the direction is changed
    ; multiple times within the same tick-period, leading to false positive
    ; crash detection. The better approach is to save the last key event, and 
    ; process it on the next tick
    (if (member ke DIRECTIONS) ke #false)

    (world-status ws)
    (world-food ws)
    ))


; WorldState -> Boolean
(define (over? ws)
  (not (string=? (world-status ws) running))
  )


; Number -> Image
; Makes an image with the score
(define (make-score s)
  (text (format "score: ~a" s) 16 "black")
  )


; WorldState -> WorldState
(define (main ws)
  (calculate-points
    (big-bang 
      ws
      [to-draw render-world]
      [on-key on-key-press]
      [on-tick tock 0.1]
      [stop-when over? render-final]
      )))


; WorldState -> Number
; Returns the points accumulated in the game state
(define (calculate-points ws)
  (- (length (world-trail ws)) (length INITIAL-SNAKE))
  )


(define 
  INITIAL-SNAKE
  (list 
    (make-posn 4 0)
    (make-posn 3 0) 
    (make-posn 2 0) 
    (make-posn 1 0) 
    (make-posn 0 0)
    )) 


(define (render-element pos col img)
  (underlay/xy
    img
    (* (posn-x pos) BLOCK-WIDTH)
    (* (posn-y pos) BLOCK-WIDTH)
    (square  ; Squares look more retro =)
      (sub1 BLOCK-WIDTH)  ; 1px gives some olc lcd feeling 
      "solid" 
      col
      )))


; Trail Image -> Image
(define (render-trail trail img)
  (local
    ((define (reduce element img)
       (render-element element SNAKE-COLOUR img)
       ))

    ; -- IN --
    (foldl reduce img trail)
    ))


; Non-empty-list-of-anything -> List-of-anything
; Drops the last element of a non empty list loa
(check-expect (drop-last (list 1 2 3)) (list 1 2))
(define (drop-last loa)
  (drop-right loa' 1)
  )


; String -> Image
(define (make-message msg)
  (text msg 24 "black")
  )


(main
  (make-world 
    INITIAL-SNAKE
    "down"
    #false
    running
    (make-posn 20 15)
    ))


(test)

