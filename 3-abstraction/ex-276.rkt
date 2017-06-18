#lang htdp/isl

(require test-engine/racket-tests)
(require 2htdp/itunes)
(require racket/list)

; ### Constants
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


(define 
  track4
  (create-track
    "Me missus, me dosh and me"
    "Marieta Marieta"
    "My previous album"
    (* 90 1000)
    1
    (create-date 1998 3 2 12 1 0)
    600
    (create-date 2016 9 1 21 0 0)
    ))

; A LTracks is one of:
; - '()
; - (cons Track LTracks)
; Example:
(define track-list (list track1 track2 track3 track4))

; ### Functions

; String Date LTracks -> LTracks
; Returns from ltracks those that belong to album and 
; have been played after the date
(check-expect
  (select-album-date "Once upon a time" (create-date 2017 9 3 21 0 0) track-list)
  (list track2)
  )
(define (select-album-date album date track-list)
  (local
    (; LTrack -> Boolean
     ; Returns whether the track matches the album
     ; and has been played after date
     (define (good? track)
       (and
         (string=? (track-album track) album)
         (date>=? (track-played track) date)
         )))

    ; -- IN --
    (filter good? track-list)
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


; LTracks -> [List-of LTracks]
; Groups tracks by their album
(check-expect 
  (select-albums track-list)
  (list
    (list track1 track2)
    (list track3)
    (list track4)
    ))
(define (select-albums track-list)
  (local
    (; String -> LTracks
     ; Returns a LTracks belonging to the album
     (define (get-album-tracks album)
       (local
         (; Track -> Boolean
          (define (good? track)
            (string=? (track-album track) album)
            ))

         ; -- IN --
         (filter good? track-list)
         ))

     (define unique-albums (remove-duplicates (map track-album track-list))
       ))

     ; -- IN --
     (map get-album-tracks unique-albums)
    ))

(test)

