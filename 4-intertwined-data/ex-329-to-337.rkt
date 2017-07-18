#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)



; ==================== Exercise 329 ====================

; Q: How many times does the file `read!` occur in 
;    the directory tree?
; A: Twice: /ts/read! and /ts/Libs/Docs/read!

; Q: What is the total size of all files in the tree?
; A: 99 + 52 + 17 + 10 + 8 + 2 + 19 = 207

; Q: Total size of directory of each directory node has size 1?
; A: 207 + 5 * 1 = 212

; Q: How many levels of directories does it contain?
; A: The deepest is /ts/Libs/Code, i.e. 3

; =================== End of exercise ==================




; ==================== Exercise 330 ====================
; ### Data Definitions

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String. 

(define dt.1
  '(("part1" "part2" "part3")
    "read!"
    (("hang" "draw")
     ("read!")
     )))

; =================== End of exercise ==================




; ==================== Exercise 331 ====================

; Dir.v1 -> N
; Counts files in `dirtree`
(check-expect (how-many dt.1) 7)
(define (how-many dirtree)
  (match dirtree
    ['() 0]
    [(cons head tail)
     (+
       (if (string? head) 1 (how-many head))
       (how-many tail)
       )]))

; =================== End of exercise ==================




; ==================== Exercise 332 ====================

; ### Data Definitions
(define-struct dir [name content])

; A Dir.v2 is a structure: 
;   (make-dir String LOFD)


; A LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
;
; Or something like [List-of File.v2|Dir.v2]

(define dt.2
  (make-dir
    "TS"
    (list
      (make-dir "Text" '("part1" "part2" "part3"))
      "read!"
      (make-dir "Libs"
        (list
          (make-dir "Code" '("hang" "draw"))
          (make-dir "Docs" '("read!"))
          )))))


; =================== End of exercise ==================




; ==================== Exercise 333 ====================

; ### Functions

; Dir.v2 -> N
; Counts files in `dtree`
(check-expect (how-many.v2 dt.2) 7)
(define (how-many.v2 dtree)
  (files-in-lofd (dir-content dtree))
  )


; LOFD -> N
; Counts files in lofd
(define (files-in-lofd lofd)
  (match lofd
    ['() 0]
    [(cons head tail)
     (+ 
       (if (string? head) 1 (how-many.v2 head))
       (files-in-lofd tail)
       )]))

; =================== End of exercise ==================




; ==================== Exercise 334 ====================

; ### Data Definitions

(define-struct dir.v2.1 [name content size readability])

; A Dir.v2.1 is a structure: 
;   (make-dir.v2.1 String LOFD Number Boolean)
;
; Example: (make-dir.v2.1 "my-dir" '() 1 #false)
;   empty directory called "my-dir" with size 1, and cannot
;   be read by anybody else but the owner

; =================== End of exercise ==================


(define-struct file [name size content])

; A File.v3 is a structure: 
;   (make-file String N String)

(define-struct dir.v3 [name dirs files])

; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)



; ==================== Exercise 335 ====================

(define dt.3
  (make-dir.v3 
    "TS"
    (list 
      (make-dir.v3
        "Text"
        '()
        (list
          (make-file "part1" 99 "")
          (make-file "part2" 52 "")
          (make-file "part3" 17 "")
          ))
      (make-dir.v3
        "Libs"
        (list
          (make-dir.v3 "Code" '() (list (make-file "hang" 8 "") (make-file "draw" 2 "")))
          (make-dir.v3 "Docs" '() (list (make-file "read!" 19 "")))
          )
        '()
        ))
    (list (make-file "read!" 10 ""))
    ))


; =================== End of exercise ==================




; ==================== Exercise 336 ====================

; ### Functions
; Dir.v3 -> N
; Counts files withing a directory tree
(check-expect (how-many.v3 dt.3) 7)
(define (how-many.v3 dtree)
  (+
    (for/sum [(d (dir.v3-dirs dtree))] (how-many.v3 d))
    (length (dir.v3-files dtree))
    ))

; Q: Why are you confident that how-many produces correct results?
; A: Because we have used the tests on this version as on the previous 
;    simpler ones

; =================== End of exercise ==================




; ==================== Exercise 337 ====================

; ### Data Definitions


; A Dir.v3 is a structure: 
;   (make-dir.v3 String [List-of Dir.v3] [List-of File])

; NOTE: A for/sum approach was used in previous example. 
;   which is probably better than a foldl/foldr with a lambda


; =================== End of exercise ==================

(test)

