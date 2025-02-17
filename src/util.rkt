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
