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
    (create-date 2017 9 3 21 30 0)
    ))

; A LTracks is one of:
; - '()
; - (cons Track LTracks)
; Example:
(define track-list (list track1 track2 track3))


; ### Functions

; String Date LTracks -> LTracks
; Extracts from list of tracks all those tracks that belong to the given album and
; have been played at date or later
(check-expect
  (select-album-date "Once upon a time" (create-date 2017 9 3 21 0 0) track-list)
  (list track2)
  )

(define (select-album-date album date track-list)
  (select-by-play-date>= date (select-album album track-list))
  )


; Date LTracks -> LTracks
; Selects tracks of which play date is equal or later to the passed date
(check-expect 
  (select-by-play-date>= (create-date 2017 9 3 21 0 0) track-list)
  (list track2 track3)
  )
(define (select-by-play-date>= date track-list)
  (cond
    [(empty? track-list) '()]
    [(date>=? (track-played (first track-list)) date)
     (cons
       (first track-list)
       (select-by-play-date>= date (rest track-list))
       )]
    [else (select-by-play-date>= date (rest track-list))]
    ))

; Date Date -> Boolean
; Returns whether the first date is equal or older than the second one
(define (date>=? a b )
  (and
    (>= (date-year a) (date-year b))
    (>= (date-month a) (date-month b))
    (>= (date-day a) (date-day b))
    (>= (date-hour a) (date-hour b))
    (>= (date-minute a) (date-minute b))
    (>= (date-second a) (date-second b))
    ))


; String LTracks -> LTracks
; Given a list of tracks, it returns all tracks that belongs to a given album
(check-expect 
  (select-album "Once upon a time" track-list)
  (list track1 track2)
  )
(check-expect 
  (select-album "My last album" track-list)
  (list track3)
  )
(define (select-album album track-list)
  (cond
    [(empty? track-list) '()]
    [(string=? (track-album (first track-list)) album) 
      (cons
        (first track-list)
        (select-album album (rest track-list))
        )]
    [else (select-album album (rest track-list))]
    ))


(require test-engine/racket-tests)
(test)
