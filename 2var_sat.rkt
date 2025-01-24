#lang typed/racket

; adding usage of Formulas and tseitin-transform
(require "tseitin.rkt")

; convert cnf to list of list of formulas -- easier to substitute i think
(define (convert [form : Formula]) : (Listof (Listof Formula))
  (match form
    [(andF forms) (convert-help forms)]
    [_ (error 'convert "invalid cnf, given ~e" form)]))

; convert helper
(define (convert-help [forms : (Listof Formula)]) : (Listof (Listof Formula))
  (match forms
    ['() '()]
    [(cons f r)
     (match f
       [(auxF fr) (cons (list f) (convert-help r))]
       [(orF frs) (cons frs (convert-help r))]
       [_ (error 'convert-help "invalid cnf, given ~e" f)])]))

; interpret CNF
(define (interp [forms : (Listof Formula)]) : Boolean
  (match forms
    [(varF var) #t]))

; brute force SAT solver given list of vars
(define (2var-sat [form : Formula] [vars : (Listof Formula)]) : Boolean
  #t)