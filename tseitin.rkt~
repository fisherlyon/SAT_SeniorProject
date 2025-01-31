#lang typed/racket

(require typed/rackunit)
(provide tseitin)
(provide Formula
         varF auxF notF andF orF condF bicondF
         varF? auxF? notF? andF? orF? condF? bicondF?)

; formula type definition
(define-type Formula (U varF auxF notF andF orF condF bicondF))
(struct varF ([var : Symbol]) #:transparent)
(struct auxF ([aux : Symbol]) #:transparent)
(struct notF ([form : Formula]) #:transparent)
(struct andF ([forms : (Listof Formula)]) #:transparent)
(struct orF ([forms : (Listof Formula)]) #:transparent)
(struct condF ([l : Formula] [r : Formula]) #:transparent)
(struct bicondF ([l : Formula] [r : Formula]) #:transparent)

; tseitin transformation (sexp -> sexp)
(define (tseitin-sexp [form : Sexp]) : Sexp
  (to-sexp (clean-cnf (to-cnf (to-auxiliary (parse form))))))

; tseitin transformation (sexp -> formula)
(define (tseitin [form : Sexp]) : Formula
  (clean-cnf (to-cnf (to-auxiliary (parse form)))))

; parses a formula (Concrete Syntax -> AST)
(define (parse [s : Sexp]) : Formula
  (match s
    [(and (not (? invalid-var? var)) (? symbol? var)) (varF var)]
    [(list '! form) (notF (parse form))]
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
    [(notF sub) (list '! (to-sexp sub))]
    [(andF forms) (cons '& (map to-sexp forms))]
    [(orF forms) (cons 'v (map to-sexp forms))]
    [(condF l r) (list '-> (to-sexp l) (to-sexp r))]
    [(bicondF l r) (list '<-> (to-sexp l) (to-sexp r))]
    [_ (error 'to-sexp "invalid AST, given ~e" form)]))

; converts a formula to conjunctive normal form
(define (to-cnf [form : Formula]) : Formula
  (match form
    [(or (? is-literal?) (? is-aux-literal?)) form]
    [(notF sub) (to-cnf (distr-neg sub))]
    [(andF subs) (andF (map to-cnf subs))]
    [(orF subs) (foldr distr-or (first (map to-cnf subs)) (rest (map to-cnf subs)))]
    [(condF l r) (to-cnf (elim-cond l r))]
    [(bicondF l r) (to-cnf (elim-bicond l r))]))

; deeply flattens nested disjunctions
(define (clean-cnf [form : Formula]) : Formula
  (define (flatten [f : Formula] [type : Symbol]) : (Listof Formula)
    (cond
      [(and (equal? type 'andF) (andF? f))
       (append-map (lambda ([sub : Formula]) (flatten sub type)) (andF-forms f))]
      [(and (equal? type 'orF) (orF? f))
       (append-map (lambda ([sub : Formula]) (flatten sub type)) (orF-forms f))]
      [else (list f)]))

  (andF 
   (map 
    (lambda ([conj : Formula]) 
      (let ([flattened (remove-duplicates (flatten conj 'orF) equal?)])
        (if (= (length flattened) 1)
            (first flattened) ; use the single element directly
            (orF flattened)))) ; otherwise, wrap in orF
    (flatten form 'andF))))

; eliminates conditional from a formula : (x -> y) <=> (!x v y)
(define (elim-cond [left : Formula] [right : Formula]) : Formula
  (orF (list (notF left) right)))

; eliminates biconditional from a formula : (x <-> y) <=> (x -> y) & (y -> x)
(define (elim-bicond [left : Formula] [right : Formula]) : Formula
  (andF (list (condF left right) (condF right left))))

; distribute negation over formula
(define (distr-neg [form : Formula]) : Formula
  (match form
    [(or (? varF?) (? auxF?)) (notF form)]
    [(notF inner) inner] ; elim double negation
    [(andF subs) (orF (map notF subs))] ; distr negative across conjunction
    [(orF subs) (andF (map notF subs))] ; distr negative across disjunction
    [(condF l r) (andF (list l (notF r)))]
    [(bicondF l r) (orF (list (notF (condF l r)) (notF (condF r l))))]
    [_ (error 'distr-neg "invalid negation, given ~e" form)]))

; distributes disjunction over conjunction
(define (distr-or [a : Formula] [b : Formula]) : Formula
  (cond
    [(andF? a) (andF (map (lambda ([f : Formula]) (distr-or f b)) (andF-forms a)))]
    [(andF? b) (andF (map (lambda ([f : Formula]) (distr-or a f)) (andF-forms b)))]
    [else (orF (list a b))]))

; converts a formula to auxiliary form
(define (to-auxiliary [form : Formula]) : Formula
  (if (or (is-literal? form) (just-literals? form))
      form
      (let* ([subforms (get-subforms form)] ; list of derived subformulas
             [aux-vars ; list of auxiliary variables
              (map (lambda ([i : Integer]) (auxF (make-var i)))
                   (build-list (length subforms) add1))])
        (andF (cons (auxF 'x1) (zip-lists (subst-subs subforms aux-vars) aux-vars))))))

; substitute subformulas for corresponding auxiliary variable
(define (subst-subs [subforms : (Listof Formula)] [aux-vars : (Listof Formula)]) : (Listof Formula)
  (define (helper [i : Integer] [j : Integer] [subd-list : (Listof Formula)]) : (Listof Formula)
    (if (>= i (length subforms))
        subd-list
        (let* ([cur-form (list-ref subd-list j)] ; form to take replacement aux
               [cur-sub (list-ref subd-list i)] ; subform to replace in form
               [cur-aux (list-ref aux-vars i)] ; aux as replacement
               [subd (substitute cur-form cur-sub cur-aux)]) ; attempted sub
          (if (equal? cur-form subd)
              (helper i (- j 1) subd-list) ; if equal, no sub, decrement j
              (helper (+ i 1) i (list-set subd-list j subd))))))
  (helper 1 0 subforms))

; substitutes a replacement for a subformula in a formula
(define (substitute [form : Formula] [sub : Formula] [repl : Formula]) : Formula
  (match form
    [(? (lambda (f) (equal? f sub))) repl] ; form = sub, so replace
    [(notF subform) (if (is-literal? subform) form (notF (substitute subform sub repl)))]
    [(or (andF subforms) (orF subforms))
     (define new-subs
       (map (lambda ([f : Formula]) (substitute f sub repl)) (cast subforms (Listof Formula))))
     (if (andF? form) (andF new-subs) (orF new-subs))]
    [(or (condF l r) (bicondF l r))
     (define new-l (substitute (cast l Formula) sub repl))
     (define new-r (substitute (cast r Formula) sub repl))
     (if (condF? form) (condF new-l new-r) (bicondF new-l new-r))]
    [_ form])) ; if no match, return the original form
 
; zip the list of subformulas and the list of auxiliary variables together
(define (zip-lists [subforms : (Listof Formula)] [aux-vars : (Listof Formula)]) : (Listof Formula)
  (map
   (lambda ([sub : Formula] [aux : Formula])
     (bicondF aux sub))
   subforms aux-vars))

; given a formula, extracts all subformulas (excluding simple variables)
(define (get-subforms [form : Formula]) : (Listof Formula)
  (match form
    [(? varF?) '()]
    [(notF sub) (cons form (get-subforms sub))]
    [(or (andF subs) (orF subs))
     (cons form (subform-list-helper (cast subs (Listof Formula))))]
    [(or (condF l r) (bicondF l r))
     (append (list form)
             (get-subforms (cast l Formula))
             (get-subforms (cast r Formula)))]))

; given a list of formulas, get all their subformulas
(define (subform-list-helper [forms : (Listof Formula)]) : (Listof Formula)
  (match forms
    ['() '()]
    [(cons f r)
     (append (get-subforms f)
             (subform-list-helper r))]))

; given the list of all subformulas, assign auxiliary variables to each
(define (give-aux [subforms : (Listof Formula)] [count : Integer]) : (Listof Formula)
  (match subforms
    ['() '()]
    [(cons f r) (cons (bicondF (auxF (make-var count)) f) (give-aux r (+ 1 count)))]))

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

; checks if a formula is a literal (assuming auxF not a literal)
(define (is-literal? [form : Formula]) : Boolean
  (match form
    [(varF _) #t]
    [(notF (varF _)) #t]
    [_ #f]))

(define (is-aux-literal? [form : Formula]) : Boolean
  (match form
    [(auxF _) #t]
    [(notF (auxF _)) #t]
    [_ #f]))

; checks if a list of formulas is a list of literals
(define (literal-list? [forms : (Listof Formula)]) : Boolean
  (match forms
    ['() #t]
    [(cons f r) (if (not (is-literal? f)) #f (literal-list? r))]))

; checks if a formula is made up of just literals
(define (just-literals? [form : Formula]) : Boolean
  (match form
    [(andF forms) (literal-list? forms)]
    [(orF forms) (literal-list? forms)]
    [(condF l r) (and (is-literal? l) (is-literal? r))]
    [(bicondF l r) (and (is-literal? l) (is-literal? r))]
    [_ #f]))

; checks if something is an invalid symbol
(define (invalid-var? [sym : Any]) : Boolean
  (match sym
    [(or '! '& 'v '-> '<->) #t]
    [_ #f]))

; test cases
; tseitin transform tests
(check-equal?
 (tseitin-sexp '(-> (-> R P) (-> (! (& Q R)) P)))
 '(&
   x1
   (v x3 (! x2) (! x1))
   (v x1 x2)
   (v x1 (! x3))
   (v P (! R) (! x2))
   (v x2 R)
   (v x2 (! P))
   (v P (! x4) (! x3))
   (v x3 x4)
   (v x3 (! P))
   (v (! x5) (! x4))
   (v x4 x5)
   (v Q (! x5))
   (v R (! x5))
   (v x5 (! R) (! Q))))

; get-subforms tests
(check-equal?
 (to-sexp (to-auxiliary (parse '(-> (v A (! B)) (<-> A (! B))))))
 '(& x1
     (<-> x1 (-> x2 x4))
     (<-> x2 (v A x3))
     (<-> x3 (! B))
     (<-> x4 (<-> A x5))
     (<-> x5 (! B))))

(check-equal?
 (to-sexp (to-auxiliary (parse '(-> (-> R P) (-> (! (& Q R)) P)))))
 '(& x1
     (<-> x1 (-> x2 x3))
     (<-> x2 (-> R P))
     (<-> x3 (-> x4 P))
     (<-> x4 (! x5))
     (<-> x5 (& Q R))))

(check-equal?
 (to-sexp (to-auxiliary (parse '(-> (& (v p q) r) (! s)))))
 '(& x1
     (<-> x1 (-> x2 x4))
     (<-> x2 (& x3 r))
     (<-> x3 (v p q))
     (<-> x4 (! s))))

(check-equal?
 (to-sexp (to-auxiliary (parse '(v (& A (! B) C) (-> D (<-> E (! F))) G))))
 '(& x1
     (<-> x1 (v x2 x4 G))
     (<-> x2 (& A x3 C))
     (<-> x3 (! B))
     (<-> x4 (-> D x5))
     (<-> x5 (<-> E x6))
     (<-> x6 (! F))))

; just-literals? tests
(check-equal?
 (just-literals? (bicondF (auxF 'x1) (condF (auxF 'x2) (auxF 'x3)))) #f)
(check-equal?
 (just-literals? (condF (varF 'R) (notF (varF 'P)))) #t)
(check-equal?
 (just-literals? (andF (list (varF 'Q) (varF 'R)))) #t)
(check-equal?
 (just-literals? (orF (list (varF 'Q) (notF (varF 'R))))) #t)

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