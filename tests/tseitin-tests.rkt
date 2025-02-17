#lang typed/racket

(require typed/rackunit)
(require "../src/tseitin.rkt")
(require "../src/formula-definitions.rkt")
(require "../src/util.rkt")

; -------------------------------------
; Test Cases for Tseitin Transformation
; -------------------------------------

; tseitin transform tests
(check-equal?
 (tseitin-sexp '(-> (-> R P) (-> (~ (& Q R)) P)))
 '(&
   x1
   (v x3 (~ x2) (~ x1))
   (v x1 x2)
   (v x1 (~ x3))
   (v P (~ R) (~ x2))
   (v x2 R)
   (v x2 (~ P))
   (v P (~ x4) (~ x3))
   (v x3 x4)
   (v x3 (~ P))
   (v (~ x5) (~ x4))
   (v x4 x5)
   (v Q (~ x5))
   (v R (~ x5))
   (v x5 (~ R) (~ Q))))

; get-subforms tests
(check-equal?
 (to-sexp (to-auxiliary (parse '(-> (v A (~ B)) (<-> A (~ B))))))
 '(& x1
     (<-> x1 (-> x2 x4))
     (<-> x2 (v A x3))
     (<-> x3 (~ B))
     (<-> x4 (<-> A x5))
     (<-> x5 (~ B))))

(check-equal?
 (to-sexp (to-auxiliary (parse '(-> (-> R P) (-> (~ (& Q R)) P)))))
 '(& x1
     (<-> x1 (-> x2 x3))
     (<-> x2 (-> R P))
     (<-> x3 (-> x4 P))
     (<-> x4 (~ x5))
     (<-> x5 (& Q R))))

(check-equal?
 (to-sexp (to-auxiliary (parse '(-> (& (v p q) r) (~ s)))))
 '(& x1
     (<-> x1 (-> x2 x4))
     (<-> x2 (& x3 r))
     (<-> x3 (v p q))
     (<-> x4 (~ s))))

(check-equal?
 (to-sexp (to-auxiliary (parse '(v (& A (~ B) C) (-> D (<-> E (~ F))) G))))
 '(& x1
     (<-> x1 (v x2 x4 G))
     (<-> x2 (& A x3 C))
     (<-> x3 (~ B))
     (<-> x4 (-> D x5))
     (<-> x5 (<-> E x6))
     (<-> x6 (~ F))))

; give-aux tests
(check-equal?
 (give-aux (list (orF (list (varF 'A) (notF (varF 'B)))) (varF 'C)) 1)
 (list
  (bicondF (auxF 'x1) (orF (list (varF 'A) (notF (varF 'B)))))
  (bicondF (auxF 'x2) (varF 'C))))
(check-equal?
 (give-aux (list (condF (notF (varF 'A)) (varF 'B)) (notF (varF 'C)) (varF 'D)) 1)
 (list
  (bicondF (auxF 'x1) (condF (notF (varF 'A)) (varF 'B)))
  (bicondF (auxF 'x2) (notF (varF 'C)))
  (bicondF (auxF 'x3) (varF 'D))))

; make-var tests
(check-equal? (make-var 1) 'x1)
(check-equal? (make-var 10) 'x10)