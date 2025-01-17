#lang typed/racket

(require typed/rackunit)

; formula type definition
(define-type Formula (U varF notF andF orF condF bicondF))
(struct varF ([var : Symbol]) #:transparent)
(struct notF ([f : Formula]) #:transparent)
(struct andF ([l : Formula] [r : Formula]) #:transparent)
(struct orF ([l : Formula] [r : Formula]) #:transparent)
(struct condF ([l : Formula] [r : Formula]) #:transparent)
(struct bicondF ([l : Formula] [r : Formula]) #:transparent)

; parses a formula
(define (parse [s : Sexp]) : Formula
  (match s
    [(and (not (? invalid-var? var)) (? symbol? var)) (varF var)]
    [(list '! form) (notF (parse form))]
    [(list '& left right) (andF (parse left) (parse right))]
    [(list 'v left right) (orF (parse left) (parse right))]
    [(list '-> left right) (condF (parse left) (parse right))]
    [(list '<-> left right) (bicondF (parse left) (parse right))]
    [_ (error 'parse "invalid syntax, given ~e" s)]))

; checks if something is an invalid symbol
(define (invalid-var? [sym : Any]) : Boolean
  (match sym
    [(or '! '& 'v '-> '<->) #t]
    [_ #f]))

; test cases
; invalid-var? tests
(check-equal? (invalid-var? '!) #t)
(check-equal? (invalid-var? '<-) #f)

; parse tests
(check-equal? (parse 'A) (varF 'A))
(check-equal? (parse '(! A)) (notF (varF 'A)))
(check-equal? (parse '(& A B)) (andF (varF 'A) (varF 'B)))
(check-equal? (parse '(v A B)) (orF (varF 'A) (varF 'B)))
(check-equal? (parse '(-> A B)) (condF (varF 'A) (varF 'B)))
(check-equal? (parse '(<-> A B)) (bicondF (varF 'A) (varF 'B)))
(check-equal? (parse '(-> (v A (! B)) (<-> A (! B))))
    (condF (orF (varF 'A) (notF (varF 'B))) (bicondF (varF 'A) (notF (varF'B)))))