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

; prepare for tseitin transform
(define (to-auxiliary [form : Formula] [count : Integer]) : Formula
  (match count
    [1 (to-auxiliary
        (bicondF (auxF (make-var count)) form) (+ 1 count))]
    [_ form])) ;; TO-DO

; (to-auxiliary (parse '(-> (v A (! B)) (<-> A (! B)))) 1)

; checks if a formula is in CNF
(define (is-cnf? [form : Formula]) : Boolean
  (match form
    [(? is-literal?) #t]
    [(orF forms) (literal-list? forms)]
    [(andF forms) (andmap is-cnf? forms)]
    [_ #f]))

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

; checks if something is an invalid symbol
(define (invalid-var? [sym : Any]) : Boolean
  (match sym
    [(or '! '& 'v '-> '<->) #t]
    [_ #f]))

; test cases
; is-cnf? tests
(check-equal? (is-cnf? (varF 'A)) #t)
(check-equal? (is-cnf? (orF (list (varF 'A) (varF 'B)))) #t)
(check-equal? (is-cnf? (andF (list (orF (list (varF 'A) (varF 'B))) (varF 'C)))) #t)
(check-equal?
 (is-cnf?
  (andF
   (list
    (orF
     (list
      (notF (andF (list (varF 'A) (varF 'B))))(varF 'C)))
    (varF 'D)))) #f)
(check-equal?
 (is-cnf?
  (andF
   (list
    (orF (list (varF 'A) (notF (varF 'B)) (notF (varF 'C))))
    (orF (list (notF (varF 'D)) (varF 'E) (varF 'F) (varF 'D) (varF 'F)))))) #t)
(check-equal?
 (is-cnf? (notF (andF (list (varF 'A) (varF 'B))))) #f)
(check-equal?
 (is-cnf? (andF (list (notF (orF (list (varF 'A) (varF 'B)))) (varF 'C)))) #f)
(check-equal?
 (is-cnf?
  (andF
   (list
    (varF 'A)
    (orF (list (varF 'B) (andF (list (varF 'D) (varF 'E)))))))) #f)

; literals-list? tests
(check-equal? (literal-list? (list (varF 'A) (notF (varF 'B)) (varF 'C))) #t)
(check-equal? (literal-list? (list (orF (list (varF 'A) (varF 'B) (varF 'C))))) #f)

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