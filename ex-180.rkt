#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; CONSTANTS
(define good (explode "good"))
(define all (explode "all"))
(define lla (explode "lla"))
(define DELETE-KEY "\u007F")

(define HEIGHT 20) 
(define WIDTH 200) 
(define FONT-SIZE 16) 
(define FONT-COLOR "black") 
 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))


; DATA DEFINITIONS
(define-struct editor [pre post])
; An Editor (or WorldState) is a structure (make-editor Lo1S Lo1S)

; An Lo1S is one of:
; - '()
; - (cons 1String Lo1S)


; FUNCTIONS
; Lo1S -> Lo1S
; Reverses a list of 1String
(check-expect (rev '()) '())
(check-expect (rev good) (explode "doog"))
(define (rev lo1s)
  (cond
    [(empty? lo1s) '()]
    [else 
      (add-at-end 
        (rev (rest lo1s)) 
        (first lo1s)
        )]))


; Lo1S 1String -> Lo1S
; Adds s to the end of lo1s
(check-expect (add-at-end '() "s") (cons "s" '()))
(check-expect (add-at-end (explode "hi") "s") (explode "his"))

(define (add-at-end lo1s s)
  (cond
    [(empty? lo1s) (cons s '())]
    [else
      (cons
        (first lo1s)
        (add-at-end (rest lo1s) s)
        )]))


; String String -> Editor
; Creates an editor from two strings
(check-expect 
  (create-editor "abc" "dfg") 
  (make-editor (explode "cba") (explode "dfg"))
  )

(check-expect
  (create-editor "abcde" "") 
  (make-editor (explode "edcba") (explode ""))
  )

(define (create-editor s1 s2)
  (make-editor
    (rev (explode s1))
    (explode s2)
    ))


; WorldState -> Image
(define (render-world ws)
  (place-image/align
    (beside 
      (editor-text (rev (editor-pre ws)))
      CURSOR
      (editor-text (editor-post ws)))
    1
    1
    "left"
    "top"
    BACKGROUND
    ))


; Lo1s -> Image
(define (editor-text lo1s)
  (cond
    [(empty? lo1s) empty-image]
    [else 
      (beside
        (text (first lo1s) FONT-SIZE FONT-COLOR)
        (editor-text (rest lo1s))
        )]))

; WorldState KeyEvent -> WorldState
; handles the key events
(check-expect (on-key-press (create-editor "" "") "u") (create-editor "u" ""))
(check-expect 
  (on-key-press (create-editor "pepe" "") "right") 
  (create-editor "pepe" "")
  )
(check-expect 
  (on-key-press (create-editor "pe" "pe") "right") 
  (create-editor "pep" "e")
  )
(check-expect 
  (on-key-press (create-editor "pe" "pe") "left") 
  (create-editor "p" "epe")
  )
(check-expect 
  (on-key-press (create-editor "" "pepe") "left") 
  (create-editor "" "pepe")
  )
(check-expect
  (on-key-press (create-editor "pepe" "") "\b") 
  (create-editor "pep" "")
  )
(check-expect
  (on-key-press (create-editor "" "pepe") "\b") 
  (create-editor "" "pepe")
  )
(check-expect
  (on-key-press (create-editor "pepe" "") "\r") 
  (create-editor "pepe" "")
  )
(check-expect
  (on-key-press (create-editor "pepe" "") "\t") 
  (create-editor "pepe" "")
  )
(define (on-key-press ws ke)
  (cond
    [(key=? ke "left") (editor-key-left ws)]
    [(key=? ke "right") (editor-key-right ws)]
    [(key=? ke "\b") (editor-backspace ws)]
    [(key=? ke DELETE-KEY) (editor-del ws)]
    [(or (key=? ke "\t") (key=? ke "\r")) ws]
    [(= (string-length ke) 1) (editor-add-letter ws ke)]
    [else ws]
    ))



; Editor -> Editor
(define (editor-key-left ed)
   (if 
     (empty? (editor-pre ed))
     ed
     (make-editor
       (rest (editor-pre ed))
       (cons (first (editor-pre ed)) (editor-post ed))
       )))


; Editor -> Editor
(define (editor-key-right ed)
  (if 
    (empty? (editor-post ed))
    ed
    (make-editor
      (cons (first (editor-post ed)) (editor-pre ed))
      (rest (editor-post ed))
      )))


; Editor -> Editor
(define (editor-backspace ed)
  (if 
    (empty? (editor-pre ed))
    ed
    (make-editor
      (rest (editor-pre ed))
      (editor-post ed)
      )))


; Editor 1String -> Editor
(define (editor-add-letter ed l)
  (make-editor
    (cons l (editor-pre ed))
    (editor-post ed)
    ))


; Editor -> Editor
(define (editor-del ed)
  (if 
    (empty? (editor-post ed))
    ed
    (make-editor
      (editor-pre ed)
      (rest (editor-post ed))
      )))


(define (main ws)
  (big-bang 
    ws
    [to-draw render-world]
    [on-key on-key-press]
    ))




; TEST & MAIN CALL
(require test-engine/racket-tests)
(test)
(main (create-editor "lolasso" "trollasso"))
