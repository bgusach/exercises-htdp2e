#lang htdp/bsl+

; ### Constants

; ### Data Definitions
(define-struct email [from date message])
; A Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time 


; List-of-email is one of:
; - '()
; - (cons Email List-of-email)


(define email1 (make-email "paco" 10 "lol"))
(define email2 (make-email "manolo" 20 "troll"))
(define email3 (make-email "mariano" 30 "huehuehue"))

; ### Functions

; List-of-email -> List-of-email
; Sorts the passed list of emails, newest first
(check-expect (sort-by-date '()) '())
(check-expect (sort-by-date (list email1 email2 email3)) (list email3 email2 email1))
(define (sort-by-date lom)
  (cond
    [(empty? lom) '()]
    [else
      (insert-by-date
        (first lom)
        (sort-by-date (rest lom))
        )]))


; Email List-of-email -> List-of-email
; Inserts email into its proper place by date given a sorted List-of-players
(check-expect (insert-by-date email1 '()) (list email1))
(check-expect (insert-by-date email2 (list email1)) (list email2 email1))
(define (insert-by-date mail lom)
  (cond
    [(empty? lom) (list mail)]
    [else
      (if 
        (email-newer? mail (first lom))
        (cons mail lom)
        (cons
          (first lom)
          (insert-by-date mail (rest lom))
          ))]))


; Email Email -> Boolean
; Returns whether the first email is newer than the second
(check-expect (email-newer? email1 email2) #false)
(check-expect (email-newer? email2 email1) #true)
(define (email-newer? a b)
  (>= (email-date a) (email-date b))
  )


; List-of-email -> List-of-email
; Sorts the passed list of emails, by the 'from' field
(check-expect (sort-by-from '()) '())
(check-expect (sort-by-from (list email1 email2 email3)) (list email2 email3 email1))
(define (sort-by-from lom)
  (cond
    [(empty? lom) '()]
    [else
      (insert-by-from
        (first lom)
        (sort-by-from (rest lom))
        )]))


; Email List-of-email -> List-of-email
; Inserts email into its proper place by date given a sorted List-of-players
(check-expect (insert-by-from email1 '()) (list email1))
(check-expect (insert-by-from email2 (list email1)) (list email2 email1))
(check-expect (insert-by-from email3 (list email2 email1)) (list email2 email3 email1))
(define (insert-by-from mail lom)
  (cond
    [(empty? lom) (list mail)]
    [else
      (if 
        (lexico-smaller? mail (first lom))
        (cons mail lom)
        (cons
          (first lom)
          (insert-by-from mail (rest lom))
          ))]))


; Email Email -> Boolean
; Returns whether the email a should be lexicographically smaller than b
; by the field 'from'
(check-expect (lexico-smaller? email2 email3) #true)
(check-expect (lexico-smaller? email3 email2) #false)
(define (lexico-smaller? a b)
  (string<? (email-from a) (email-from b))
  )


(require test-engine/racket-tests)
(test)

