#lang typed/racket

(require typed/rackunit)

; formula type definition
(define-type Formula (U varF auxF notF andF orF condF bicondF))
(struct varF ([var : Symbol]) #:transparent)
(struct auxF ([var : Symbol]) #:transparent)
(struct notF ([form : Formula]) #:transparent)
(struct andF ([forms : (Listof Formula)]) #:transparent)
(struct orF ([forms : (Listof Formula)]) #:transparent)
(struct condF ([l : Formula] [r : Formula]) #:transparent)
(struct bicondF ([l : Formula] [r : Formula]) #:transparent)

; parses a formula
(define (parse [s : Sexp]) : Formula
  (match s
    [(and (not (? invalid-var? var)) (? symbol? var)) (varF var)]
    [(list '! form) (notF (parse form))]
    [(list '& forms ...) (andF (map parse forms))]
    [(list 'v forms ...) (orF (map parse forms))]
    [(list '-> left right) (condF (parse left) (parse right))]
    [(list '<-> left right) (bicondF (parse left) (parse right))]
    [_ (error 'parse "invalid syntax, given ~e" s)]))

; checks if a formula is in CNF
(define (check-cnf [form : Formula]) : Boolean
  (match form
    [(varF _) #t]
    [(notF (varF _)) #t]
    [(auxF _) #t]
    [(orF forms) (literal-list? forms)]
    [(andF forms) (disjunct-literal-list? forms)]
    [(condF _ _) #f]
    [(bicondF _ _) #f]))

; makes an auxilary variable symbol
(define (make-var [n : Integer]) : Symbol
  (string->symbol (format "x~a" n)))

; checks if a formula is a literal
(define (is-literal? [form : Formula]) : Boolean
  (match form
    [(varF _) #t]
    [(auxF _) #t]
    [(notF (varF _)) #t]
    [_ #f]))

; checks if a list of formulas is a list of literals
(define (literal-list? [forms : (Listof Formula)]) : Boolean
  (match forms
    ['() #t]
    [(cons f r) (if (not (is-literal? f)) #f (literal-list? r))]))

; checks if a formula is a disjunct
(define (is-disjunct? [form : Formula]) : Boolean
  (match form
    [(orF _) #t]
    [_ #f]))

; checks if a list of formulas is a list of disjuncts
(define (disjunct-literal-list? [forms : (Listof Formula)]) : Boolean
  (match forms
    ['() #t]
    [(cons f r) (if (not (or (is-literal? f) (is-disjunct? f))) #f (disjunct-literal-list? r))]))

; checks if something is an invalid symbol
(define (invalid-var? [sym : Any]) : Boolean
  (match sym
    [(or '! '& 'v '-> '<->) #t]
    [_ #f]))

; test cases
; check-cnf tests
(check-equal? (check-cnf (varF 'A)) #t)
(check-equal? (check-cnf (orF (list (varF 'A) (varF 'B)))) #t)
(check-equal? (check-cnf (andF (list (orF (list (varF 'A) (varF 'B))) (varF 'C)))) #t)

; disjuncts-list? tests
(check-equal? (disjunct-literal-list? (list (varF 'A) (notF (varF 'B)) (varF 'C))) #t)
(check-equal?
 (disjunct-literal-list?
  (list (orF (list (varF 'A) (varF 'B) (varF 'C))) (andF (list (varF 'D) (varF 'E))))) #f)
(check-equal?
 (disjunct-literal-list?
  (list (orF (list (varF 'A) (varF 'B) (varF 'C))) (orF (list (varF 'D) (varF 'E))))) #t)

; literals-list? tests
(check-equal? (literal-list? (list (varF 'A) (notF (varF 'B)) (varF 'C))) #t)
(check-equal? (literal-list? (list (orF (list (varF 'A) (varF 'B) (varF 'C))))) #f)

; is-disjunct? tests
(check-equal? (is-disjunct? (orF (list (varF 'A) (varF 'B) (varF 'C)))) #t)
(check-equal? (is-disjunct? (andF (list (varF 'A) (varF 'B) (varF 'C)))) #f)

; is-literal? tests
(check-equal? (is-literal? (varF 'A)) #t)
(check-equal? (is-literal? (notF (varF 'A))) #t)
(check-equal? (is-literal? (andF (list (varF 'A) (varF 'B) (varF 'C)))) #f)

; make-var tests
(check-equal? (make-var 1) 'x1)
(check-equal? (make-var 10) 'x10)

; invalid-var? tests
(check-equal? (invalid-var? '!) #t)
(check-equal? (invalid-var? '<-) #f)

; parse tests
(check-equal? (parse 'A) (varF 'A))
(check-equal? (parse '(! A)) (notF (varF 'A)))
(check-equal? (parse '(& A B C)) (andF (list (varF 'A) (varF 'B) (varF 'C))))
(check-equal? (parse '(v A B C)) (orF (list (varF 'A) (varF 'B) (varF 'C))))
(check-equal? (parse '(-> A B)) (condF (varF 'A) (varF 'B)))
(check-equal? (parse '(<-> A B)) (bicondF (varF 'A) (varF 'B)))
(check-equal?
 (parse '(-> (v A (! B)) (<-> A (! B))))
 (condF (orF (list (varF 'A) (notF (varF 'B)))) (bicondF (varF 'A) (notF (varF'B)))))