#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)
(require htdp/dir)
(require racket/string)
(require racket/list)

; ==================== Exercise 338 ====================

; NOTE: at least for the moment, the tests must be run in this dir
(define dummy-dir (create-dir "./dummy-dir"))


; ### Functions
; Dir -> N
; Counts files withing a directory tree
(check-expect (how-many dummy-dir) 3)
(define (how-many dtree)
  (+
    (for/sum [(d (dir-dirs dtree))] (how-many d))
    (length (dir-files dtree))
    ))

; Q: Why are you confident that how-many produces correct 
;    results for these directories?
; A: Because I created a test directory only for these tests

; =================== End of exercise ==================




; ==================== Exercise 339 ====================

; Dir String -> Boolean
; Returns whether the 
(check-expect (find? dummy-dir "proper-programme.php") #false)  ; no way :)
(check-expect (find? dummy-dir "lol.txt") #true)  ; no way :)
(check-expect (find? dummy-dir "important-stuff") #true)  ; no way :)
(define (find? dir name)
  (or
    (ormap (λ (file) (string=? (file-name file) name)) (dir-files dir))
    (ormap (λ (subdir) (find? subdir name)) (dir-dirs dir))
    ))

; =================== End of exercise ==================




; ==================== Exercise 340 ====================

; Dir -> [List-of String]
; Returns all names of files and dirs within dir (not recursively)
(check-expect (ls dummy-dir) '("subdir" "README" "lol.txt"))
(define (ls dir)
  (append
    (for/list [(d (dir-dirs dir))] (clean-dir-name (dir-name d)))
    (for/list [(f (dir-files dir))] (file-name f))
    ))

; Symbol -> String
; Does some work so that the dir name looks the same in linux and windows
(define (clean-dir-name name)
  (last
    (string-split
      (string-replace
        (string-replace (symbol->string name) "\\" "/")
        "|"
        ""
        )
      "/"
      )))

; =================== End of exercise ==================




; ==================== Exercise 341 ====================

; Dir -> N
; Returns the size of the directory tree
(check-expect (du dummy-dir) 155)
(define (du dir)
  (+ 
    (for/sum [(file (dir-files dir))] (file-size file))
    (for/sum [(subdir (dir-dirs dir))] (add1 (du subdir)))
    ))

; =================== End of exercise ==================


; A Path is [List-of String].
; interpretation: directions into in a directory tree


; ==================== Exercise 342 ====================

; Dir String -> Path
; Given a file name, it returns the path to the first file
; with that name, or #false if not found
(check-expect (find dummy-dir "hehehe") #false)
(check-expect (find dummy-dir "lol.txt") '("lol.txt"))
(check-expect (find dummy-dir "important-stuff") '("subdir" "important-stuff"))
(define (find dir name)
  (local
    ((define file-here? 
       (ormap 
         (λ (f) (string=? (file-name f) name))
         (dir-files dir)
         )))

    ; -- IN --
    (if 
      file-here? 
      (list name)

      (for/or [(subdir (dir-dirs dir))]
        (local
          ((define subres (find subdir name)))

          ; -- IN --
          (if 
            (false? subres)
            #false
            (cons (clean-dir-name (dir-name subdir)) subres)
            ))))))

; Hint from exercise: While it is tempting to first check whether the file name
; occurs in the directory tree, you have to do so for every single
; sub-directory. Hence it is better to combine the functionality of find? and
; find.
;
; Comment: yes, but anyway you have to retraverse in full depth the tree to
; find where the file is. I think it is clearer and more efficient not to use 
; find?


; Dir String -> [List-of Path]
; Finds all the occurences of `name` in `dir`
(define (find-all dir name)
  (local
    ((define local-matches
      (foldl 
        (λ (file acc) 
           (if 
             (string=? (file-name file) name) 
             (cons file acc)
             acc
             )))))

    ; -- IN --
    (append 
      local-matches
      (foldl 
        (λ (subdir acc) ()) ...
        '(....
        (dir-dirs dir)
      )
  )


; =================== End of exercise ==================

(test)

