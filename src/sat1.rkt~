#lang typed/racket

(require "sat-util.rkt")
(require "read-cnf.rkt")

; ---------------
; ---- SAT-I ----
; ---------------

; Returns a full variable instantiation since we are traversing
; the tree all the way to the leaves.

; kb = knowledge base (∆)
; n = number of variabels
; d = depth (initially set to -1)
; P = given variables; assuming P_1, P_2, ..., P_n
; L = the accumulated full variable instantiation
(define (sat1 [kb : (Listof (Listof Integer))] [n : Integer] [d : Integer] [P : (Listof Integer)] [L : (Listof Integer)]) : (U Boolean (Listof Integer))
  (cond
    [(and (equal? d (- n 1)) (equal? kb '{})) (reverse L)]
    [(and (equal? d (- n 1)) (equal? kb '{{}})) #f]
    [else
     (let ([pos-result (sat1 (condition kb (list-ref P (+ d 1))) n (+ d 1) P (cons (list-ref P (+ d 1)) L))])
       (if (not (equal? pos-result #f))
           pos-result
           (sat1 (condition kb (- (list-ref P (+ d 1)))) n (+ d 1) P (cons (- (list-ref P (+ d 1))) L))))]))

; allows .cnf file input upon run
(define (main) : Void
  (define args (vector->list (current-command-line-arguments)))
  (define n (length args))
  (if (or (> n 2) (< n 1))
      (printf "Usage: ./sat1 <in_filename> [<out_filename>]\n")
      (let-values ([(cnf num-vars) (parse-file (first args))])
        (define result (sat1 cnf num-vars -1 (build-list num-vars add1) '()))
        (define (output-fun) : Void
          (match result
            [#f (printf "UNSAT\n")]
            [(list p ...)
             (printf "SAT\n")
             (printf "~a0\n" (tva->string (apply list p)))]))
        (if (= n 2)
            (with-output-to-file (list-ref args 1) output-fun #:exists 'replace)
            (output-fun)))))

(main)