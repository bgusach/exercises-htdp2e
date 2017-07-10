#lang htdp/isl+


; Figure 101
(define (insertion-sort alon)
  (local 
  ; === Scope of main `alon`, local `sort` and local `add` ========
                                                                  ;
    ((define (sort alon)                                          ;
       ; === Scope of local `alon` ============================   ;
       (cond                                                  ;   ; 
         [(empty? alon) '()]                                  ;   ; 
         [else (add (first alon) (sort (rest alon)))]         ;   ; 
       ; ======================================================   ; 
         ))                                                       ;
                                                                  ;
      (define (add an alon)                                       ;
        ; === Scope of local `alon` and `an` ==================   ;
        (cond                                                 ;   ;
          [(empty? alon) (list an)]                           ;   ;
          [else                                               ;   ; 
            (cond                                             ;   ; 
              [(> an (first alon)) (cons an alon)]            ;   ;
              [else (cons (first alon) (add an (rest alon)))] ;   ;
        ; =====================================================   ;
              )])))                                               ;
                                                                  ;
    (sort alon)                                                   ;
                                                                  ;
  ; ===============================================================
    ))

; Figure 102
(define (sort alon)
  (local                                                            
  ; === Scope of main `alon`, local `sort` and local `add` ========
                                                                  ;
    ((define (sort alon)                                          ;
        ; === Scope of local `alon` ===========================   ; 
        (cond                                                 ;   ;
          [(empty? alon) '()]                                 ;   ;
          [else (add (first alon) (sort (rest alon)))]        ;   ;
        ; =====================================================   ;
          ))                                                      ;
                                                                  ;
     (define (add an alon)                                        ;
       ;=== Scope of local `alon` and `an` ====================   ;
       (cond                                                  ;   ;
         [(empty? alon) (list an)]                            ;   ; 
         [else                                                ;   ;
           (cond                                              ;   ;
             [(> an (first alon)) (cons an alon)]             ;   ;
             [else (cons (first alon) (add an (rest alon)))]  ;   ;
                                                              ;   ;
       ;=======================================================   ;
             )])))                                                ;
                                                                  ;
    ; -- IN --                                                    ;
    (sort alon)                                                   ;
    ; =============================================================
    ))

; NOTE: Both `insertion-sort` and `sort` are bound to the top-level scope

; Q: Do the two functions differ other than in name?
; A: Apparently they don't. 
