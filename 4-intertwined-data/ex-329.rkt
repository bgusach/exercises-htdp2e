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


(test)
