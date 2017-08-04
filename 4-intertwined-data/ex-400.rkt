#lang htdp/isl+

(require test-engine/racket-tests)
(require racket/list)
(require racket/string)
(require racket/base)


; ==================== Exercise 400 ====================

; ### Data Definitions

; DNASymbol is one of:
; - 'a
; - 'c
; - 'g
; - 't


; [List-of DNASymbol] [List-of DNASymbol] -> Boolean
; Returns whether search-str starts with the given pattern
(check-expect (DNAPrefix '(a c g) '(a c g g g)) #true)
(check-expect (DNAPrefix '() '(a c g g g)) #true)
(check-expect (DNAPrefix '(a c) '(g g g)) #false)
(check-expect (DNAPrefix '(a c) '(a)) #false)
(define (DNAPrefix pattern search-str)
  (cond
    [(empty? pattern) #true]
    [(empty? search-str) #false]
    [else
      (and
        (symbol=? (first pattern) (first search-str))
        (DNAPrefix (rest pattern) (rest search-str))
        )]))


; [List-of DNASymbol] [List-of DNASymbol] 
;   -> [List-of DNASymbol] or Error or Boolean
(check-expect (DNAdelta '(a c g) '(a c g g g)) '(g g))
(check-expect (DNAdelta '() '(a c g)) '(a c g))
(check-expect (DNAdelta '(a c) '(g g g)) #false)
(check-expect (DNAdelta '(a c) '(a)) #false)
(check-error (DNAdelta '(a c) '(a c)))
(define (DNAdelta pattern search-str)
  (local
    ((define pat-empty (empty? pattern))
     (define search-empty (empty? search-str))
     )

    ; -- IN --
    (cond
      [(and pat-empty search-empty) (error "lists identical")]
      [(and pat-empty (not search-empty)) search-str]
      [(and (not pat-empty) search-empty) #false]
      [else
        (if 
          (symbol=? (first pattern) (first search-str))
          (DNAdelta (rest pattern) (rest search-str))
          #false
          )])))

; =================== End of exercise ==================


(test)

