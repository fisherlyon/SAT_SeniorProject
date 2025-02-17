#lang typed/racket

(require typed/rackunit)
(require "../src/formula-definitions.rkt")
(require "../src/util.rkt")

; Test Cases for Utility Functions

; parse tests
(check-equal? (parse 'A) (varF 'A))
(check-equal? (parse '(~ A)) (notF (varF 'A)))
(check-equal? (parse '(& A B C)) (andF (list (varF 'A) (varF 'B) (varF 'C))))
(check-equal? (parse '(v A B C)) (orF (list (varF 'A) (varF 'B) (varF 'C))))
(check-equal? (parse '(-> A B)) (condF (varF 'A) (varF 'B)))
(check-equal? (parse '(<-> A B)) (bicondF (varF 'A) (varF 'B)))
(check-equal?
 (parse '(-> (v A (~ B)) (<-> A (~ B))))
 (condF (orF (list (varF 'A) (notF (varF 'B)))) (bicondF (varF 'A) (notF (varF'B)))))

; to-sexp tests
(check-equal?
 (to-sexp (bicondF (auxF 'x1) (condF (varF 'A) (notF (varF 'B)))))
 '(<-> x1 (-> A (~ B))))
(check-equal?
 (to-sexp (parse '(-> (v A (~ B)) (<-> A (~ B)))))
 '(-> (v A (~ B)) (<-> A (~ B))))
(check-equal?
 (to-sexp (parse '(v (& A (~ B) C) (-> D (<-> E (~ F))) G)))
 '(v (& A (~ B) C) (-> D (<-> E (~ F))) G))
(check-equal?
 (to-sexp (parse '(-> (-> R P) (-> (~ (& Q R)) P))))
 '(-> (-> R P) (-> (~ (& Q R)) P)))
(check-equal?
 (to-sexp (parse '(-> (& (v p q) r) (~ s))))
 '(-> (& (v p q) r) (~ s)))

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

; is-literal? tests
(check-equal? (is-literal? (varF 'A)) #t)
(check-equal? (is-literal? (notF (varF 'A))) #t)
(check-equal? (is-literal? (auxF 'x1)) #f)
(check-equal? (is-literal? (andF (list (varF 'A) (varF 'B) (varF 'C)))) #f)

; is-aux-literal tests
(check-equal? (is-aux-literal? (auxF 'x1)) #t)
(check-equal? (is-aux-literal? (notF (auxF 'x1))) #t)
(check-equal? (is-aux-literal? (varF 'A)) #f)
(check-equal? (is-aux-literal? (andF (list (auxF 'x1) (auxF 'x2) (auxF 'x3)))) #f)

; literals-list? tests
(check-equal? (literal-list? (list (varF 'A) (notF (varF 'B)) (varF 'C))) #t)
(check-equal? (literal-list? (list (orF (list (varF 'A) (varF 'B) (varF 'C))))) #f)

; just-literals? tests
(check-equal?
 (just-literals? (bicondF (auxF 'x1) (condF (auxF 'x2) (auxF 'x3)))) #f)
(check-equal?
 (just-literals? (condF (varF 'R) (notF (varF 'P)))) #t)
(check-equal?
 (just-literals? (andF (list (varF 'Q) (varF 'R)))) #t)
(check-equal?
 (just-literals? (orF (list (varF 'Q) (notF (varF 'R))))) #t)

; invalid-var? tests
(check-equal? (invalid-var? '~) #t)
(check-equal? (invalid-var? '<-) #f)