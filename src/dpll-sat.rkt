#lang typed/racket

(require "sat-util.rkt")

; ------------------
; ---- DPLL-SAT ----
; ------------------

; kb = knowledge base (∆)
; I = a set of literals that were either present as unit clauses in kb (∆)
;     or derived  by unit resolution
; G = a new knowledge base which results from conditioning kb (∆) on I
(define (dpll-sat [kb : (Listof (Listof Integer))]) : (U Boolean (Listof Integer))
  (define-values (I G) (unit-res kb '()))
  (cond
    [(equal? G '{}) I]
    [(equal? G '{{}}) #f]
    [else
     (define L (MOM G))
     (let ([pos-result (dpll-sat (cons (list L) G))])
       (if (not (equal? pos-result #f))
           (cons L I)
           (let ([neg-result (dpll-sat (cons (list (- L)) G))])
             (if (not (equal? neg-result #f))
                 (cons (- L) I)
                 #f))))]))