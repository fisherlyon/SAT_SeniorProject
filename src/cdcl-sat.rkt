#lang typed/racket

(require "sat-util.rkt")
(require "read-cnf.rkt")

; ------------------
; ---- CDCL-SAT ----
; ------------------

; kb = knowledge base (∆)
; D = a decision sequence
; G = a set of learned clauses
(define (cdcl-sat [kb : (Listof (Listof Integer))]) : Boolean
  (define D : (Boxof (Listof Integer)) (box '()))
  (define G : (Listof (Listof Integer)) '())
  (define I : (Listof Integer) '())
  (define (while-true [kb : (Listof (Listof Integer))]) : Boolean
    (let-values ([(I result) (cdcl-unit-res kb G (unbox D) I)])
      (if (equal? result '{{}}) ; if unit resolution detects a contradiction
          (if (equal? (unbox D) '()) ; contradiction without any decisions
              #f
              #t)
          #t))
;;     (if (not (cdcl-unit-res kb G (unbox D))) ; if unit res detects a contradiction
;;         (if (equal? (unbox D) '())
;;             #f
;;             (let* ([a (get-asserting-clause ...)]
;;                    [m (get-assertion-level ...)])
;;               (erase m d)
;;               (cons a G)))
;;         (if (); l is a literal where neither l or ¬l are implied by unit res
;;             (add l d)
;;             #t))
    )
  (while-true kb))

; Unit Resolution for CDCL
(define (cdcl-unit-res [kb : (Listof (Listof Integer))] ; knowledge base
                       [G : (Listof (Listof Integer))] ; set of learned clauses
                       [D : (Listof Integer)] ; decision sequence
                       [I : (Listof Integer)]) ; implications based on unit resolution
  : (Values (Listof Integer) (Listof (Listof Integer)))
  (define temp (append kb G (ints->clauses D)))
  (define unit-clause (find-unit-clause temp))
  (if (equal? unit-clause '{})
      (values I kb)
      (cdcl-unit-res
       (condition temp (first unit-clause))
       G
       '()
       (cons (first unit-clause) I))))

; Implication Graph -- should return an asserting clause
(define (impl-graph [kb : (Listof (Listof Integer))] ; knowledge base
                    [D : (Listof Integer)]) ; decision sequence
  : (Listof Integer)
  '())

; Turns a list of integers into a list of unit (integer) clauses
(define (ints->clauses [unit-clauses : (Listof Integer)]) : (Listof (Listof Integer))
  (match unit-clauses
    ['() '()]
    [(cons f r) (cons (list f) (ints->clauses r))]))

; Erase decisions after the given assertion level (m)
(define (erase [m : Nonnegative-Integer] [D : (Boxof (Listof Integer))]) : Void
  (set-box! D (take (unbox D) (min (length (unbox D)) (add1 m)))))

; Adds an element to a mutable list (Boxof (Listof ...))
(define (add [l : Integer] [D : (Boxof (Listof Integer))]) : Void
  (set-box! D (append (unbox D) (list l))))