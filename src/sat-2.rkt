#lang typed/racket

; ----------------
; ---- SAT-II ----
; ----------------

; Returns a partial variable instantiation since we are not traversing
; the tree all the way to the leaves.
; To get a TVA, it isn't always necessary to traverse the tree fully.

; kb = knowledge base (∆)
; n = number of variabels
; d = depth (initially set to -1)
; p = given variables; assuming P_1, P_2, ..., P_n
(define (sat2 [kb : (Listof (Listof Integer))] [n : Integer] [d : Integer] [P : (Listof Integer)] [L : (Listof Integer)]) : (U Boolean (Listof Integer))
  (cond
    [(equal? kb '{}) (reverse L)]
    [(equal? kb '{{}}) #f]
    [else
     (let ([pos-result (sat2 (condition kb (list-ref P (+ d 1))) n (+ d 1) P (cons (list-ref P (+ d 1)) L))])
       (if (not (equal? pos-result #f))
           pos-result
           (sat2 (condition kb (- (list-ref P (+ d 1)))) n (+ d 1) P (cons (- (list-ref P (+ d 1))) L))))]))

; condition a knowledge base on a literal
; "condition ∆ on P_n"
; kb = knowledge base (∆)
; l = literal
(define (condition [kb : (Listof (Listof Integer))] [l : Integer]) : (Listof (Listof Integer))
  (match kb
    ['{} '{}]
    [(cons f r)
     (match (condition-help f l)
       [#t (condition r l)] ; clause = #t, remove clause from kb
       ['{} '{{}}] ; empty clause, contradiction
       [(list p ...) (cons (apply list p) (condition r l))])]))

; conditions a single clause on a literal
(define (condition-help [clause : (Listof Integer)] [l : Integer]) : (U Boolean (Listof Integer))
  (match clause
    ['{} '{}] ; empty clause, contradiction
    [(cons f r)
     (cond
       [(equal? f l) #t] ; P_d = l, clause = #t
       [(equal? f (- l)) (condition-help r l)] ; P_d = -l, remove from clause
       [else
        (match (condition-help r l)
          [#t #t]
          [(list p ...) (cons f (apply list p))])])])) ; P_d != l, continue