#lang htdp/bsl+

(require test-engine/racket-tests)


(check-expect
  (cons "a" (list 0 #false))
  (list "a" 0 #false)
  )


(check-expect
  (cons "a" (list 0 #false))
  (cons "a" (cons 0 (cons #false '())))
  )


(check-expect
  (list (cons 1 (cons 13 '())))
  (list (list 1 13))
  )


(check-expect
  (list (cons 1 (cons 13 '())))
  (cons (cons 1 (cons 13 '())) '())
  )


(check-expect
  (cons (list 1 (list 13 '())) '())
  (list (list 1 (list 13 '())))
  )


(check-expect
  (cons (list 1 (list 13 '())) '())
  (cons 
    (cons 
      1 
      (cons 
        (cons 
          13 
          (cons 
            '() 
            '()
            )) 
        '()
        )) 
    '()
    ))


(test)
