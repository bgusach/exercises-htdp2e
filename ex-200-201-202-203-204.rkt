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




; ==================== Exercise 200 ====================
; ### Functions

; LTracks -> Number
; Calculates the total amount of play time of a track list
(check-expect (total-time track-list) (+ (track-time track1) (track-time track2) (track-time track3)))
(define (total-time track-list)
  (cond
    [(empty? track-list) 0]
    [else
      (+
        (track-time (first track-list))
        (total-time (rest track-list))
        )]))

; =================== End of exercise ==================




; ==================== Exercise 201 ====================
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
  (select-album-titles/unique track-list)
  (list "Once upon a time" "My last album")
  )
(define (select-album-titles/unique track-list)
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

; =================== End of exercise ==================




; ==================== Exercise 202 ====================
; ### Functions

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

; =================== End of exercise ==================




; ==================== Exercise 203 ====================
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

; =================== End of exercise ==================




; ==================== Exercise 204 ====================
; ### Functions

; LTracks -> List-of-LTracks
; Groups tracks by their album
(check-expect 
  (select-albums track-list)
  (list
    (list track1 track2)
    (list track3)
    ))
(define (select-albums track-list)
  (select-tracks-by-albums (select-album-titles/unique track-list) track-list)
  )


; List-of-strings LTrack -> List-of-Ltrack
; Given a list of album names and a list of tracks, it groups the tracks by album name
(check-expect 
  (select-tracks-by-albums (list (track-album track1) (track-album track3)) track-list)
  (list 
    (list track1 track2)
    (list track3)
    ))
(define (select-tracks-by-albums albums track-list)
  (cond
    [(empty? albums) '()]
    [else 
      (cons
        (select-album (first albums) track-list)
        (select-tracks-by-albums (rest albums) track-list)
        )]))

; =================== End of exercise ==================



(require test-engine/racket-tests)
(test)

