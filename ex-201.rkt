#lang htdp/bsl+

(require 2htdp/itunes)

; ### Data Definitions

; A Date is a structure:
;   (make-date N N N N N N)
; interpretation An instance records six pieces of information:
; year, 
; month (between 1 and 12 inclusive), 
; day (between 1 and 31), 
; hour (between 0 and 23),
; minute (between 0 and 59),
; and second (also between 0 and 59).


; A Track is a structure:
;   (make-track String String String N N Date N Date)
; interpretation An instance records in order: 
; - the track's title, 
; - artist, 
; - album
; - length in milliseconds, 
; - position within the album
; - the date it was added
; - how often it has been played, 
; - and the date when it was last played
; Examples:
(define 
  track1 
  (create-track
    "Pacman in the dark"
    "Johnny and the drunken squirrels"
    "Once upon a time"
    (* 3 60 1000)
    1
    (create-date 2015 6 3 12 30 30)
    42
    (create-date 2017 9 2 21 45 15)
    ))

(define 
  track2
  (create-track
    "My little goat"
    "Johnny and the drunken squirrels"
    "Once upon a time"
    (* 2 60 1000)
    2
    (create-date 2014 7 2 12 0 0)
    93
    (create-date 2017 9 3 21 0 0)
    ))

(define 
  track3
  (create-track
    "Eating cookies after the dusk"
    "Marieta Marieta"
    "My last album"
    (* 180 1000)
    1
    (create-date 2000 3 2 12 1 0)
    600
    (create-date 2017 9 3 21 0 0)
    ))

; A LTracks is one of:
; - '()
; - (cons Track LTracks)
; Example:
(define track-list (list track1 track2 track3))


; ### Functions

; LTracks -> List-of-Strings
; Returns all albums titles from list of tracks. Repeated albums will show up multiple times
(check-expect 
  (select-all-album-titles track-list)
  (list "Once upon a time" "Once upon a time" "My last album")
  )
(define (select-all-album-titles track-list)
  (cond
    [(empty? track-list) '()]
    [else
      (cons
        (track-album (first track-list))
        (select-all-album-titles (rest track-list))
        )]))


; LTracks -> List-of-Strings
; Returns all albums titles from list of tracks. Repeated albums will show up only once
(check-expect 
  (select-all-album-titles/unique track-list)
  (list "Once upon a time" "My last album")
  )
(define (select-all-album-titles/unique track-list)
  (remove-repeated-strings 
    (select-all-album-titles track-list)
    ))


; List-of-strings -> List-of-strings
; Removes duplicated strings from a list
(define (remove-repeated-strings los)
  (cond
    [(empty? los) '()]
    [(member (first los) (rest los)) 
     (remove-repeated-strings (rest los))]
    [else 
      (cons
        (first los)
        (remove-repeated-strings (rest los))
        )]))


(require test-engine/racket-tests)
(test)

