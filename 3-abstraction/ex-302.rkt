#lang htpd/isl+

; (define x (cons 1 x))

; Q: to what value is x bound?
; A: I think it is a free occurrence, i.e. it is bound to no value
;    and therefore cannot be handled (at least by isl+). This holds
;    true for local definitions as well. 

;    Even if x was bound to a proper value on the top-level scope, 
;    a local redefinition of x based on x could not be carried out.
