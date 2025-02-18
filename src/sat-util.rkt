#lang typed/racket

(provide (all-defined-out))

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

; Return Values:
; I = a set of literals that were either present as unit clauses in kb (∆)
;     or derived  by unit resolution
; G = a new knowledge base which results from conditioning kb (∆) on I
(define (unit-res [kb : (Listof (Listof Integer))]) : (Values (Listof Integer) (Listof (Listof Integer)))
  (values '() '()))