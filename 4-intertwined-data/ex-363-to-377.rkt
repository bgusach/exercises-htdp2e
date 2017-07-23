#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)


; ### Data definitions

; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())


; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])


; An Xexpr.v2 is a list: 
; – (cons Symbol [List-of Xexpr.v2])
; – (cons Symbol (cons [List-of Attribute] [List-of Xexpr.v2]))

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))


; ==================== Exercise 363 ====================

; An Xexpr.v2 is a (cons Symbol XE-Body)


; An XE-Body (short for XML expression body)
; is one of:
; - [List-of Xexpr.v2]
; - '(cons [List-of Attribute] [List-of Xexpr.v2])


; An Attribute is a pair (list of two items):
;   (cons Symbol (cons String '()))

; =================== End of exercise ==================




; ==================== Exercise 364 ====================

; 1: <transition from="seen-e" to="seen-f" />
'(transition ((from "seen-e") (to "seenf")))

; 2: <ul><li><word /><word /></li><li><word /></li></ul>
'(ul
   (li (word) (word))
   (li (word))
   )

; Q: Which one could be represented in Xexpr.v0 or Xexpr.v1?
; A: Xexpr.v0 only supports one node without subnodes or
;    attributes, therefore neither 1 nor 2 can be represented
;    with that definition.
;    Xexpr.v1 supports subnodes, and therefore can be used
;    for 2, but not for 1, because it does not allow attributes.
;    

; =================== End of exercise ==================




; ==================== Exercise 365 ====================

; 1: '(server ((name "example.org"))
;    <server name="example.org" />

; 2: '(carcas (board (grass)) (player ((name "sam"))))
;    <carcas>
;      <board>
;        <grass />
;      </board>
;      <player name="sam" />
;    </carcas>

; 3: '(start)
;    <start />

; Q: Which ones are elements of Xexpr.v0 or Xexpr.v1?
; A: 1 -> is neither
;    2 -> Xexpr.v1
;    3 -> Xexpr.v0 

; =================== End of exercise ==================




; ==================== Exercise 366 ====================

(define a0 '((initial "X")))
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; NOTE: does not belong to the exercise, but xexpr-attr has
; been rewritten

; Xexpr.v2 -> [List-of Attribute]
; Retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))
(define (xexpr-attr xe)
  (local
    ((define attr+children (rest xe)))

    ; -- IN --
    (match
      attr+children
      ['() '()]
      [(cons head _)
       (if (list-of-attributes? head) head '())
       ]
      [else '()]
      )))


; [List-of Attribute] or Xexpr.v2 -> Boolean
; Returns whether the given value is a list of attributes
(define (list-of-attributes? x)
  (match
    x
    ['() #true]
    [(cons (? cons?) _) #true]
    [else #false]
    ))


; Xexpr.v2 -> Symbol
; Returns the name of `xe`
(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e3) 'machine)
(check-expect (xexpr-name e4) 'machine)
(check-error (xexpr-name a0) "Expected Xexpr, got ((initial \"X\")) instead")
(define (xexpr-name xe)
  (match
    xe
    [(cons (? symbol?) _) (first xe)]
    [else (error (format "Expected Xexpr, got ~s instead" xe))]
    ))


; Xexpr.v2 -> XEConts
; Returns the contents (i.e. children) of `xe`
(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))
(check-error 
  (xexpr-content "broken") 
  "Expected Xexpr, got \"broken\" instead"
  )
(define (xexpr-content xe)
  (local
    ((define _ (xexpr-name xe))  ; Just to crash gracefully
     (define loa+content (rest xe))
     )

     ; -- IN --
     (match
       loa+content
       ['() '()]
       [(cons head tail)
        (if 
          (list-of-attributes? head)
          tail
          loa+content
          )])))

; =================== End of exercise ==================




; ==================== Exercise 367 ====================

; Q: Explain why xexpr-attr does not need a self-reference
; A: The recipe calls for recursion when dealing with
;    self-referencing data structures as lists are.
; 
;    However, lists are not used here as an arbitrarily 
;    large data structure, but as a "shorthand" for 
;    structures (i.e. first attribute is pos 0, second 1, etc)
;    Therefore it does not make sense to apply the same
;    function to all the elements of the "list as a structure".
;    Each element of the list as a different whole meaning.
;    In other languages, tuples would be used instead of lists.

; =================== End of exercise ==================




; ==================== Exercise 368 ====================

; An [Either A B] is a one of:
; - A
; - B

; An AttributesOrXexpr is a:
;  [Either [List-of Attribute] Xexpr.v2]

; With this new Data Definition, the function
; signature of xexpr-attr would look like this:
; AttributesOrXexpr -> Boolean

; =================== End of exercise ==================




; ==================== Exercise 369 ====================

; [List-of Attribute] Symbol -> [Maybe String]
(check-expect (find-attr a0 'initial) "X")
(check-expect (find-attr a0 'not-existing) #false)
(check-expect (find-attr '((w 39) (h 84) (b 99)) 'b) 99)
(define (find-attr attrs name)
  (local
    ((define res (assq name attrs)))

    ; -- IN --
    (if 
      (false? res) 
      #false
      (second res)
      )))


; =================== End of exercise ==================


; NOTE: from now on, Xexpr refers to Xexpr.v2

; ### Data Definitions
; An XWord is '(word ((text String))).
; 
; Its XML node would be: <word text="...">



; ==================== Exercise 370 ====================

(define word0 '(word ((text "hello"))))
(define word1 '(word ((text "schnitzel"))))
(define word2 '(word ((text "paella"))))


; [X] X -> Boolean
(check-expect (word? word0) #true)
(check-expect (word? word1) #true)
(check-expect (word? '(this is no word)) #false)
(check-expect (word? '(word "neither this")) #false)
(define (word? x)
  (match
    x
    [(cons 'word (cons attrs '()))
     (and
       (list-of-attributes? attrs)
       (symbol=? (caar attrs) 'text)
       (string? (cadar attrs))
       )]
    [else #false]
    ))


; XWord -> String
(check-expect (word-text word0) "hello")
(define (word-text w)
  (if
    (word? w)
    (second (first (second w)))
    (error (format "expected word, got ~a" w))
    ))


; =================== End of exercise ==================




; ==================== Exercise 371 ====================

; An Xexpr is a one of:
; - XWord
; - (cons Symbol XE-Body)

; NOTE: I'm not sure this is the right answer.


; =================== End of exercise ==================


; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))

; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))


; ==================== Exercise 372 ====================


; ### Constants

(define BULLET (circle 5 "solid" "black"))
(define FONT-SIZE 30)
(define FONT-COLOUR 'black)

(define li-lol '(li (word ((text "lol")))))
(define li-troll '(li (word ((text "troll")))))

(define rendered-lol 
  (beside/align 'center BULLET (text "lol" FONT-SIZE FONT-COLOUR))
  )
(define rendered-troll
  (beside/align 'center BULLET (text "troll" FONT-SIZE FONT-COLOUR))
  )

; ### Functions


; Image -> N
; Shows the passed image for debugging purposes
(define (show-img img)
  (animate (λ (n) img))
  )


; XItem.v1 -> Image
; Renders a <li> item
(check-expect 
  (render-item1 li-lol)
  rendered-lol
  )
(check-expect 
  (render-item1 li-troll)
  rendered-troll
  )
(define (render-item1 i)
  (local 
    ((define content (xexpr-content i))
     (define element (first content))
     (define a-word (word-text element))
     (define item (text a-word FONT-SIZE FONT-COLOUR))
     )

    ; -- IN --
    (beside/align 'center BULLET item)
    ))


; =================== End of exercise ==================




; ==================== Exercise 373 ====================

; ### Constants

; – (cons 'ul [List-of XItem.v1])
(define ul `(ul ,li-lol ,li-troll))

(define rendered-ul
  (above/align 
    'left
    rendered-lol
    rendered-troll
    ))


; ### Functions

; XEnum.v1 -> Image 
; Renders a simple enumeration as an image 
(check-expect (render-enum.v1 ul) rendered-ul)
(define (render-enum.v1 xe) 
  (foldl
    (λ (li img) (above/align 'left img (render-item1 li)))
    empty-image
    (xexpr-content xe)
    ))

; (show-img (render-enum.v1 ul))

; =================== End of exercise ==================


; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))


; ==================== Exercise 373 ====================

(define ul2 
  `(ul 
     (li (word ((text "hola"))))
     (li (word ((text "amigo"))))
     (li ,ul)
     ))


; Image -> Image
; Adds a bullet to the left of the image
(define (bulletize item)
  (beside/align 'center BULLET item)
  )


; String -> Img
; Converts a string into an image
(define (text->img str)
  (text str FONT-SIZE 'black)
  )
 

(define rendered-ul2
  (above/align
    'left
    empty-image
    (above/align
      'left
      (bulletize (text->img "hola"))
      (above/align
        'left
        (bulletize (text->img "amigo"))
        (bulletize rendered-ul)
        ))))
          

; XEnum.v2 -> Image
(check-expect 
  (render-enum.v2 ul2)
  rendered-ul2
  )
(define (render-enum.v2 xe) 
  (foldl
    (λ (li img) (above/align 'left img (render-item.v2 li)))
    empty-image
    (xexpr-content xe)
    ))


; XItem.v2 -> Image
; Renders one XItem.v2 as an image 
(check-expect
  (render-item.v2 '(li (word ((text "aapsis")))))
  (bulletize (text->img "aapsis"))
  )
(define (render-item.v2 an-item)
  (local 
    ((define content (first (xexpr-content an-item))))

     ; -- IN --
     (bulletize
      (cond
        [(word? content)
         (text->img (word-text content))
         ]
        [else (render-enum.v2 content)]
        ))))


; (show-img (render-enum.v2 ul2))

; =================== End of exercise ==================




; ==================== Exercise 374 ====================

; An XItem.v3 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (cons XWord '())))
; – (cons 'li (cons XEnum.v3 '()))
; – (cons 'li (cons [List-of Attribute] (cons XEnum.v3 '())))
; 
; An XEnum.v3 is one of:
; – (cons 'ul [List-of XItem.v3])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v3]))


(check-expect 
  (render-enum.v3 ul2)
  rendered-ul2
  )
(define (render-enum.v3 ul)
  (foldl
    (λ (li img) (above/align 'left img (render-item.v3 li)))
    empty-image
    (xexpr-content ul)
    ))


(check-expect
  (render-item.v3 '(li (word ((text "melon")))))
  (bulletize (text->img "melon"))
  )
(define (render-item.v3 li)
  (local
    ((define contents (first (xexpr-content li))))

    ; -- IN --
    (bulletize
      (if 
        (word? contents)
        (text->img (word-text contents))
        (render-enum.v3 contents)
        ))))


; (show-img (render-enum.v3 ul2))

; =================== End of exercise ==================




; ==================== Exercise 375 ====================

(check-expect
  (render-item.v4 '(li (word ((text "melon")))))
  (bulletize (text->img "melon"))
  )
(define (render-item.v4 li)
  (local
    ((define contents (first (xexpr-content li))))

    ; -- IN --
    (if 
      (word? contents)
      (bulletize (text->img (word-text contents)))
      (bulletize (render-enum.v3 contents))
      )))

; Q: Why are you confident that your change works? 
; A: Well, the tests tell me it works, and it is
;    an example of "distribute law"... if each
;    branch has consists of calculating some value
;    and passing it to bulletize, it is the same
;    as passing to bulletize the result of the 
;    expression. 
; Q: Which version do you prefer?
; A: Not sure. Having the function wrapping the
;    if/cond expression is smart and saves typing
;    but it somehow goes against the idea of
;    analyzing items/types upfront and 
;    then processing them (as suggested in the 
;    design recipe). In general I would go for
;    clarity and letting each branch completely
;    build the result. Moreover, it is more flexible,
;    i.g. if we decided later on we want to apply 
;    another bullet to the nested lists, it would
;    be easier of each branch is 100% responsible
;    of the returned value.


; =================== End of exercise ==================




; ==================== Exercise 376 ====================

; XEnum.v2 -> N
; Counts the "Hello" items within the enumeration
(check-expect (count-hello-ul '(ul)) 0)
(check-expect 
  (count-hello-ul '(ul (li (word ((text "hello"))))))
  1
  )
(check-expect 
  (count-hello-ul 
    '(ul 
       (li (word ((text "hello"))))
       (li 
         (ul 
           (li (word ((text "amigo"))))
           (li (word ((text "hello"))))
           ))))
  2
  )
(define (count-hello-ul ul)
  (for/sum [(li (xexpr-content ul))] (count-hello-li li))
  )


(define (count-hello-li li)
  (local
    ((define contents (first (xexpr-content li))))

    (if
      (word? contents)
      (if (string=? (word-text contents) "hello") 1 0)
      (count-hello-ul contents)
      )))

; =================== End of exercise ==================




; ==================== Exercise 377 ====================


; XEnum.v2 -> XEnum.v2
; Replaces all "hello" items with "bye"
(check-expect (hello->bye-ul '(ul)) '(ul))
(check-expect 
  (hello->bye-ul '(ul (li (word ((text "hello"))))))
  '(ul (li (word ((text "bye")))))
  )
(check-expect 
  (hello->bye-ul 
    '(ul 
       (li (word ((text "hello"))))
       (li 
         (ul 
           (li (word ((text "amigo"))))
           (li (word ((text "hello"))))
           ))))
  '(ul 
     (li (word ((text "bye"))))
     (li 
       (ul 
         (li (word ((text "amigo"))))
         (li (word ((text "bye"))))
         ))))
(define (hello->bye-ul ul)
  `(ul
     ,@(for/list [(li (xexpr-content ul))]
        (hello->bye-li li)
        )))


; XItem.v2 -> XItem.v"
; Replaces all "hello"s with "bye"s in the li
; including sub uls
(define (hello->bye-li li)
  (local
    ((define contents (first (xexpr-content li))))

    (if
      (word? contents)
      `(li 
         (word 
           ((text 
              ,(if 
                 (string=? (word-text contents) "hello")
                 "bye"
                 (word-text contents)
                 )))))
      `(li ,(hello->bye-ul contents))
      )))

; =================== End of exercise ==================

(test)

