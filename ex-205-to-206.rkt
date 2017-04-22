#lang htdp/bsl+

(require 2htdp/itunes)
(require test-engine/racket-tests)


; ==================== Exercise 205 ====================
; ### Data Definitions
; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date

; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
 
; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)
; For example (same data-examples as previous exercises):
(define 
  lassoc1 
  (list
    (list "name" "Pacman in the dark")
    (list "artist" "Johnny and the drunken squirrels")
    (list "album" "Once upon a time")
    (list "time" (* 3 60 1000))
    (list "track#" 1)
    (list "added" (create-date 2015 6 3 12 30 30))
    (list "play#" 42)
    (list "played" (create-date 2017 9 2 21 45 15))
    ))

(define 
  lassoc2
  (list
    (list "name" "My little goat")
    (list "artist" "Johnny and the drunken squirrels")
    (list "album" "Once upon a time")
    (list "time" (* 2 60 1000))
    (list "track#" 2)
    (list "added" (create-date 2014 7 2 12 0 0))
    (list "play#" 93)
    (list "played" (create-date 2017 9 3 21 0 0))
    ))

(define 
  lassoc3
  (list
    (list "name" "Eating cookies after the dusk")
    (list "artist" "Marieta Marieta")
    (list "album" "My last album")
    (list "time" (* 180 1000))
    (list "track#" 1)
    (list "added" (create-date 2000 3 2 12 1 0))
    (list "play#" 600)
    (list "played" (create-date 2017 9 3 21 0 0))
    ))

(define 
  lassoc4
  (list
    (list "name" "Me missus, me dosh and me")
    (list "artist" "Marieta Marieta")
    (list "album" "My previous album")
    (list "time" (* 90 1000))
    (list "track#" 1)
    (list "added" (create-date 1998 3 2 12 1 0))
    (list "play#" 600)
    (list "played" (create-date 2016 9 1 21 0 0))
    ))


; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
; For example
(define llist (list lassoc1 lassoc2 lassoc3 lassoc4))

; =================== End of exercise ==================




; ==================== Exercise 206 ====================
; String LAssoc Any -> Any
; Given a LAssoc and a key, it returns the value if key pressent in the lassoc
; otherwise it returns the default
(check-expect (find-association "name" lassoc4 "default") (second (first lassoc4)))
(check-expect (find-association "non-existing-key" lassoc4 "default") "default")
(define (find-association key lassoc default)
  (cond
    [(empty? lassoc) default]
    [(string=? (first (first lassoc)) key) (second (first lassoc))]
    [else (find-association key (rest lassoc) default)]
    ))


; Alternative implementation of find-association based on assoc
(check-expect (find-association-v2 "name" lassoc4 "default") (second (first lassoc4)))
(check-expect (find-association-v2 "non-existing-key" lassoc4 "default") "default")
(define (find-association-v2 key lassoc default)
  (value-or-default (assoc key lassoc) default)
  )


; Assoc|Boolean Any -> Any
; If given an Assoc structure, it returns its value. If #false passed, default is returned
(define (value-or-default pair default)
  (if 
    (false? pair)
    default
    (second pair)
    ))

; =================== End of exercise ==================




; ==================== Exercise 207 ====================


; =================== End of exercise ==================


(test)

