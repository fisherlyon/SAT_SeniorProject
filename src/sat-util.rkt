#lang typed/racket

(provide (all-defined-out))

; formats verbose result
; assumes result is sorted
(define (format [result : (Listof Integer)] [vars : (Listof String)]) : (Listof String)
  (match result
    ['() '()]
    [(cons f r)
     (cons
      (if (< f 0)
          (string-append (list-ref vars (abs (+ f 1))) " = False")
          (string-append (list-ref vars (- f 1)) " = True"))
      (format r vars))]))

; condition a knowledge base on a literal
; "condition ∆ on P_n"
; kb = knowledge base (∆)
; l = literal
(define (condition [kb : (Listof (Listof Integer))] [l : Integer]) : (Listof (Listof Integer))
  (match kb
    ['{} '{}]
    ['{{}} '{{}}]
    [(cons f r)
     (match (condition-help f l)
       [#t (condition r l)] ; clause = #t, remove clause from kb
       ['{} '{{}}] ; empty clause, contradiction
       [(list p ...)
        (let ([rest (condition r l)])
          (if (equal? rest '{{}}) ; check if the next call contains a contradiction
              '{{}}
              (cons (apply list p) rest)))])]))

; conditions a single clause on a literal
(define (condition-help [clause : (Listof Integer)] [l : Integer]) : (U Boolean (Listof Integer))
  (match clause
    ['{} '{}] ; empty clause, contradiction
    [(cons f r)
     (cond
       [(equal? f l) #t] ; P_d = l, clause = #t
       [(equal? f (- l)) (condition-help r l)] ; P_d = -l, remove from clause
       [else
        (match (condition-help r l)
          [#t #t]
          [(list p ...) (cons f (apply list p))])])])) ; P_d != l, continue

; Return Values:
; I = a set of literals that were either present as unit clauses in kb (∆)
;     or derived  by unit resolution
; G = a new knowledge base which results from conditioning kb (∆) on I
(define (unit-res [kb : (Listof (Listof Integer))] [I : (Listof Integer)])
  : (Values (Listof Integer) (Listof (Listof Integer)))
  (define unit-clause (find-unit-clause kb))
  (if (equal? unit-clause '{})
      (values I kb)
      (unit-res (condition kb (first unit-clause)) (cons (first unit-clause) I))))

; Finds unit clauses given a knowledge base
(define (find-unit-clause [kb : (Listof (Listof Integer))]) : (Listof Integer)
  (match kb
    ['() '()]
    [(cons f r) (if (equal? (length f) 1) f (find-unit-clause r))]))

; MOM Heuristic: Maximum Occurrence in Minimum-sized Clauses
; the variable ordering heuristic of choice for use in DPLL
(define (MOM [kb : (Listof (Listof Integer))]) : Integer
  (most-freq-var (find-small-clauses kb (find-small-clause-size kb))))

; Finds the length of the smallest clause in a knowldedge base
(define (find-small-clause-size [kb : (Listof (Listof Integer))]) : Integer
  (match kb
    ['{} 0]
    [(cons f r) (if (equal? r '{}) 
                    (length f) 
                    (min (length f) (find-small-clause-size r)))]))

; Finds all of the clauses of length k in the knowledge base
(define (find-small-clauses [kb : (Listof (Listof Integer))] [k : Integer]) : (Listof (Listof Integer))
  (match kb
    ['{} '{}]
    [(cons f r) (if (equal? (length f) k)
                    (cons f (find-small-clauses r k))
                    (find-small-clauses r k))]))

; Finds the most frequent literal given a list of clauses
(define (most-freq-var [clauses : (Listof (Listof Integer))]) : Integer
  (define ht : (HashTable Integer Integer) (make-hash))
  (for ([clause clauses])
    (for ([literal clause])
      (hash-update! ht (abs literal) add1 (lambda () 0)))) ; want variable, not just literal
    (mfv-help ht))

; Helper for most-frequent-literal
(define (mfv-help [ht : (HashTable Integer Integer)]) : Integer
  (define literal 0)
  (define max 0)
  (for ([(key value) (in-hash ht)])
    (if (> value max)
        (begin
          (set! max value)
          (set! literal key))
        (void)))
  literal)

; turns a list of integers to a strings of integers
(define (tva->string [tva : (Listof Integer)]) : String
  (match tva
    ['() ""]
    [(cons f r) (string-append (number->string f) " " (tva->string r))]))

; Turns a list of integers into a list of unit (integer) clauses
(define (ints->clauses [unit-clauses : (Listof Integer)]) : (Listof (Listof Integer))
  (match unit-clauses
    ['() '()]
    [(cons f r) (cons (list f) (ints->clauses r))]))

; Set Difference Function
(define (set-diff [A : (Listof Integer)] [B : (Listof Integer)])
  (filter (lambda (x) (not (member x B))) A))

; Erase decisions after the given assertion level (m)
(define (erase [m : Integer] [D : (Boxof (Listof Integer))]) : Void
  (if (equal? m -1)
      (set-box! D '())
      (set-box! D (take (unbox D) (min (length (unbox D)) (add1 m))))))

; Adds an element to a mutable list (Boxof (Listof ...))
(define (add [l : Integer] [D : (Boxof (Listof Integer))]) : Void
  (set-box! D (append (unbox D) (list l))))

; Returns true/false if a list contains a given element
(define (contains [lst : (Listof Integer)] [elem : Integer])
  (not (false? (member elem lst))))