#lang htdp/bsl+

(require 2htdp/batch-io)
(require 2htdp/itunes)

; ### Constants
(define 
  track1 
  (create-track
    "Mi carro"
    "Don Manuel Escobar"
    "Manolo's greatest hits"
    (* 3 60 1000)
    1
    (create-date 2015 6 3 12 30 30)
    42
    (create-date 2017 9 2 21 45 15)
    ))

(define 
  track2
  (create-track
    "Donde estará mi carro"
    "Don Manuel Escobar"
    "Manolo's greatest hits"
    (* 3 60 1000)
    1
    (create-date 2015 6 3 12 30 30)
    42
    (create-date 2017 9 2 21 45 15)
    ))

; ### Data Definitions

; ### Functions
track1
track2

(require test-engine/racket-tests)
(test)

