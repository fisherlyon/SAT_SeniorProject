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
  (if (or (> n 3) (< n 1))
      (printf "Usage: ./sat1 [-v] <in_filename> [<out_filename>]\n")
      (let ([v (if (equal? (first args) "-v") 1 0)])
        (let-values ([(cnf num-vars vars) (parse-file (list-ref args (+ 0 v)))])
          (define result (sat1 cnf num-vars -1 (build-list num-vars add1) '()))
          (define (output-fun) : Void
            (match result
              [#f (printf "UNSAT\n")]
              [(list p ...)
               (printf "SAT\n")
               (define sorted (sort (apply list p) (lambda ([x : Integer] [y : Integer]) (< (abs x) (abs y)))))
               (printf "~a0\n" (tva->string sorted))
               (if (and (equal? v 1) (not (equal? vars '())))
                   (let ([formatted-result (format sorted vars)])
                     (printf "--------------\nVerbose Output\n--------------\n")
                     (for ([res (in-list formatted-result)])
                       (printf "~a\n" res)))
                   (void))]))
          (if (equal? n (+ 2 v))
              (with-output-to-file (list-ref args (+ 1 v)) output-fun #:exists 'replace)
              (output-fun))))))

(main)