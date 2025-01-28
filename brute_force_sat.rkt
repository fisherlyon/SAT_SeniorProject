#lang typed/racket

(require typed/rackunit)
(require "tseitin.rkt") ; adding usage of Formulas and tseitin

; brute force SAT solver for a given sexp formula -- work in progress
(define (bf-sat [form : Sexp]) : (U (Listof Symbol) (Listof Boolean))
  (let* ([rf-form (reformat (tseitin form))]
         [vars (extract-vars rf-form '())]
         [var-subs (build-list (length vars) add1)])
    '(#t)))

; given a reformatted cnf, substitutes tva (truth value assignment) for variable
(define (bool-sub [cnf : (Listof (Listof Formula))] [vars : (Listof Symbol)] [tvas : (Listof Boolean)]) : (Listof (Listof Boolean))
  (match cnf
    ['() '()]
    ))

;(define (interp [forms : (Listof (Listof Formula))] [vars : (List of Symbol)]) : Boolean
;  )

; extract variables from cnf and sort them
(define (extract-vars [cnf : (Listof (Listof Formula))] [result : (Listof Symbol)]) : (Listof Symbol)
  (match cnf
    ['() (sort result symbol<?)]
    [(cons f r)
     (extract-vars
      r
      (foldl (lambda ([form : Formula] [acc : (Listof Symbol)])
               (match form
                 [(varF var) (if (member var acc) acc (cons var acc))]
                 [(auxF var) (if (member var acc) acc (cons var acc))]
                 [(notF (varF var)) (if (member var acc) acc (cons var acc))]
                 [(notF (auxF var)) (if (member var acc) acc (cons var acc))]
                 [_ (error 'extract-vars "invalid cnf, given ~e" form)]))
             result
             f))]))

; reformat cnf to list of list of formulas
(define (reformat [form : Formula]) : (Listof (Listof Formula))
  (match form
    [(andF forms)
     (foldl (lambda ([f : Formula] [acc : (Listof (Listof Formula))])
              (match f
                [(auxF var) (cons (list f) acc)]
                [(orF frs) (cons frs acc)]
                [_ (error 'reformat "invalid cnf, given ~e" f)]))
            '()
            forms)]))

; tests
; extract-vars tests
(check-equal?
 (extract-vars (reformat (tseitin '(-> (& (v p q) r) (! s)))) '())
 '(p q r s x1 x2 x3 x4))

(check-equal?
 (extract-vars (reformat (tseitin '(v (& A (! B) C) (-> D (<-> E (! F))) G))) '())
 '(A B C D E F G x1 x2 x3 x4 x5 x6))

; reformat tests
(check-equal?
 (reformat (tseitin '(-> (& (v p q) r) (! s))))
 (list
  (list (auxF 'x4) (varF 's))
  (list (notF (varF 's)) (notF (auxF 'x4)))
  (list (auxF 'x3) (notF (varF 'q)))
  (list (auxF 'x3) (notF (varF 'p)))
  (list (varF 'q) (varF 'p) (notF (auxF 'x3)))
  (list (auxF 'x2) (notF (varF 'r)) (notF (auxF 'x3)))
  (list (varF 'r) (notF (auxF 'x2)))
  (list (auxF 'x3) (notF (auxF 'x2)))
  (list (auxF 'x1) (notF (auxF 'x4)))
  (list (auxF 'x1) (auxF 'x2))
  (list (auxF 'x4) (notF (auxF 'x2)) (notF (auxF 'x1)))
  (list (auxF 'x1))))