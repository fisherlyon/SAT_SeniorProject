#lang typed/racket

(require "sat-util.rkt")
(require "read-cnf.rkt")

; ------------------
; ---- DPLL-SAT ----
; ------------------

; kb = knowledge base (∆)
; I = a set of literals that were either present as unit clauses in kb (∆)
;     or derived  by unit resolution
; G = a new knowledge base which results from conditioning kb (∆) on I
(define (dpll-sat [kb : (Listof (Listof Integer))]) : (U Boolean (Listof Integer))
  (define-values (I G) (unit-res kb '())) ; unit resolution on the knowledge base
  (cond
    [(equal? G '{}) I] ; base case
    [(equal? G '{{}}) #f] ; base case
    [else
     (define L (MOM G)) ; choose a literal L in G
     (let ([pos-result (dpll-sat (cons (list L) G))])
       (if (not (boolean? pos-result))
           (append pos-result I)
           (let ([neg-result (dpll-sat (cons (list (- L)) G))])
             (if (not (boolean? neg-result))
                 (append neg-result I)
                 #f))))]))

; allows .cnf file input upon run
(define (main) : Void
  (define args (vector->list (current-command-line-arguments)))
  (define n (length args))
  (if (or (> n 2) (< n 1))
      (printf "Usage: ./dpll-sat <in_filename> [<out_filename>]\n")
      (let-values ([(cnf num-vars) (parse-file (first args))])
        (define result (dpll-sat cnf))
        (define (output-fun) : Void
          (match result
            [#f (printf "UNSAT\n")]
            [(list p ...)
             (printf "SAT\n")
             (printf "~a0\n"
                     (tva->string (sort (apply list p) (lambda ([x : Integer] [y : Integer]) (< (abs x) (abs y))))))]))
        (if (= n 2)
            (with-output-to-file (list-ref args 2) output-fun #:exists 'replace)
            (output-fun)))))

(main)