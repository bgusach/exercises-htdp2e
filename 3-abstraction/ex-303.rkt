#lang htdp/isl+

; NOTE: Each lambda function is written twice. Once for the binding variable of x
; and another one for the lexical scopes

; ITEM 1
; X binding
(lambda (x y)
  ;      ^----------
  (+ x (* x y))    ;
  ;  |--bound-to----
  )

; Scopes
(lambda (x y)
  ; === Scope of local `x`, `y` ==
  (+ x (* x y))                  ;
  ; ==============================
  )


; ITEM 2
; X bindings
(lambda (x y)    
  ;      ^--------
  (+             ;
    x ; ----------
    (local 

      ((define x (* y y)))
      ;        ^------------
                           ;                
      ; -- IN --           ;
      (+ (* 3 x) (/ 1 x))  ;
      ;       |-------|-----
      )))

; Scopes
(lambda (x y)    
  ; === Scope of local `x`, `y` ========
  (+                                   ;
    x                                  ;
    (local                             ;
      ; === Scope of local `x` ====    ;
      ((define x (* y y)))        ;    ;
                                  ;    ;
      ; -- IN --                  ;    ;
      (+ (* 3 x) (/ 1 x))         ;    ;
      ; ===========================    ;
      )))                              ;
                                       ;
  ; ===================================;



; ITEM 3
; X Bindings
(lambda (x y)
  ;      ^------
  (+           ;
    x          ;
  ; |-----------
    ((lambda (x) (+ (* 3 x) (/ 1 x)))
     ;        ^          |       |
     ;        |------------------- 
     (* y y))
    ))

; Scopes
(lambda (x y)
  ; === Scope of local binding `x` and `y` ====
  (+                                          ;
    x                                         ;
    ((lambda (x)                              ;
     ; === Scope of local binding `x` ====    ;
       (+ (* 3 x) (/ 1 x)))              ;    ;
     ; ===================================    ;
                                              ;
     (* y y))                                 ;
  ; ===========================================
    ))
