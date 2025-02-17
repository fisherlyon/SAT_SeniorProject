#lang typed/racket

(require typed/rackunit)
(require "form-def.rkt")
(require "util.rkt")
(provide (all-defined-out))

; ----------------------
; Tseitin Transformation
; ----------------------

; tseitin transformation (sexp -> sexp)
(define (tseitin-sexp [form : Sexp]) : Sexp
  (define p-form (parse form))
  (if (is-cnf? p-form)
      form
      (to-sexp (clean-cnf (to-cnf (to-auxiliary p-form))))))

; tseitin transformation (sexp -> formula)
(define (tseitin [form : Sexp]) : Formula
  (define p-form (parse form))
  (if (is-cnf? p-form)
      p-form
      (clean-cnf (to-cnf (to-auxiliary p-form)))))

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

; eliminates conditional from a formula : (x -> y) <=> (~x v y)
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
(define (give-aux [subforms : (Listof Formula)] [acc : Integer]) : (Listof Formula)
  (match subforms
    ['() '()]
    [(cons f r) (cons (bicondF (auxF (make-var acc)) f) (give-aux r (+ 1 acc)))]))

; makes an auxilary variable symbol
(define (make-var [n : Integer]) : Symbol
  (string->symbol (format "x~a" n)))
