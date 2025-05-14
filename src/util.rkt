#lang typed/racket

(require "form-def.rkt")
(provide (all-defined-out))

; -----------------
; Utility Functions
; -----------------

; Parses a Formula (Concrete Syntax -> AST)
(define (parse [s : Sexp]) : Formula
  (match s
    [(and (not (? invalid-var? var)) (? symbol? var)) (varF var)]
    [(list '~ form) (notF (parse form))]
    [(list '& forms ...) (andF (map parse forms))]
    [(list 'v forms ...) (orF (map parse forms))]
    [(list '-> left right) (condF (parse left) (parse right))]
    [(list '<-> left right) (bicondF (parse left) (parse right))]
    [_ (error 'parse "invalid syntax, given ~e" s)]))

; AST -> Concrete Syntax
(define (to-sexp [form : Formula]) : Sexp
  (match form
    [(varF var) var]
    [(auxF aux) aux]
    [(notF sub) (list '~ (to-sexp sub))]
    [(andF forms) (cons '& (map to-sexp forms))]
    [(orF forms) (cons 'v (map to-sexp forms))]
    [(condF l r) (list '-> (to-sexp l) (to-sexp r))]
    [(bicondF l r) (list '<-> (to-sexp l) (to-sexp r))]
    [_ (error 'to-sexp "invalid AST, given ~e" form)]))

; Checks if a given Formula is already in CNF
(define (is-cnf? [form : Formula]) : Boolean
  (match form
    [(? is-literal?) #t]
    [(orF forms) (literal-list? forms)]
    [(andF forms) (andmap is-cnf? forms)]
    [_ #f]))

; Checks if a given Formula is a literal (assuming auxF not a literal)
(define (is-literal? [form : Formula]) : Boolean
  (match form
    [(varF _) #t]
    [(notF (varF _)) #t]
    [_ #f]))

; Checks if a given Formula is an auxiliary literal
(define (is-aux-literal? [form : Formula]) : Boolean
  (match form
    [(auxF _) #t]
    [(notF (auxF _)) #t]
    [_ #f]))

; Checks if a given list of Formulas is a list of literals
(define (literal-list? [forms : (Listof Formula)]) : Boolean
  (match forms
    ['() #t]
    [(cons f r) (if (not (is-literal? f)) #f (literal-list? r))]))

; Checks if a given formula is just made up of just logical connectives and literals
(define (just-literals? [form : Formula]) : Boolean
  (match form
    [(andF forms) (literal-list? forms)]
    [(orF forms) (literal-list? forms)]
    [(condF l r) (and (is-literal? l) (is-literal? r))]
    [(bicondF l r) (and (is-literal? l) (is-literal? r))]
    [_ #f]))

; Checks if something is an invalid symbol
(define (invalid-var? [sym : Any]) : Boolean
  (match sym
    [(or '~ '& 'v '-> '<->) #t]
    [_ #f]))

; reformat cnf to list of list of formulas
(define (reformat [form : Formula]) : (Listof (Listof Formula))
  (match form
    [(andF forms)
     (foldl (lambda ([f : Formula] [acc : (Listof (Listof Formula))])
              (match f
                [(varF var) (cons (list f) acc)]
                [(notF (varF var)) (cons (list f) acc)]
                [(auxF var) (cons (list f) acc)]
                [(orF frs) (cons frs acc)]
                [_ (error 'reformat "invalid cnf, given ~e" f)]))
            '()
            forms)]))

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

; sub in numbers for variables in the reformatted cnf
(define (subst-nums-for-vars [forms : (Listof (Listof Formula))] [vars : (Listof Symbol)] [nums : (Listof Integer)]) : (Listof (Listof Integer))
  (match forms
    ['() '()]
    [(cons f r) (cons (single-form-subst f vars nums) (subst-nums-for-vars r vars nums))]))

; subs in a list of numbers for a list of variables in a formula
(define (single-form-subst [form : (Listof Formula)] [vars : (Listof Symbol)] [nums : (Listof Integer)]) : (Listof Integer)
  (match (list vars nums)
    [(list '() '()) '()]
    [(list (cons f1 r1) (cons f2 r2)) (append (single-num-var-subst form f1 f2) (single-form-subst form r1 r2))]))

; helper for subst numbers -- subs in a number for a variable in a formula
(define (single-num-var-subst [form : (Listof Formula)] [var : Symbol] [num : Integer]) : (Listof Integer)
  (match form
    ['() '()]
    [(cons f r)
     (match f
       [(varF v) (if (equal? v var)
                     (cons num (single-num-var-subst r var num))
                     (single-num-var-subst r var num))]
       [(auxF v) (if (equal? v var)
                     (cons num (single-num-var-subst r var num))
                     (single-num-var-subst r var num))]
       [(notF (varF v)) (if (equal? v var)
                            (cons (- num) (single-num-var-subst r var num))
                            (single-num-var-subst r var num))]
       [(notF (auxF v)) (if (equal? v var)
                            (cons (- num) (single-num-var-subst r var num))
                            (single-num-var-subst r var num))]
       [_ (error 'single-num-var-subst "invalid var, given ~e" f)])]))
