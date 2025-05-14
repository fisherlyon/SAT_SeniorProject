#lang typed/racket

(require typed/rackunit)
(require "form-def.rkt")
(require "tseitin.rkt") ; adding usage of Formulas and tseitin
;(provide reformat extract-vars build-list subst-nums-for-vars interp get-sat-result bin-to-bool padded-binary)

; gathers the result
(define (get-result [vars : (Listof Symbol)] [tva : (Listof Boolean)]) : (Listof (Pairof Symbol Boolean))
  (match (list vars tva)
    [(list '() '()) '()]
    [(list (cons f1 r1) (cons f2 r2)) (cons (cons f1 f2) (get-result r1 r2))]))

; gathers the result for read_cnf
(define (get-sat-result [vars : (Listof Integer)] [tva : (Listof Boolean)]) : String
  (match (list vars tva)
    [(list '() '()) ""]
    [(list (cons f1 r1) (cons f2 r2))
     (if f2
         (string-append (number->string f1) " " (get-sat-result r1 r2))
         (string-append (number->string (- f1)) " " (get-sat-result r1 r2)))]))





