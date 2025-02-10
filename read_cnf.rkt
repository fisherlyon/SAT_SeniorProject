#lang typed/racket

#|
The core purpose of this file is to take in a DIMACS .cnf file and run
the brute force SAT on it.
|#

(require typed/rackunit)
(require "tseitin.rkt")
(require "brute_force_sat.rkt")


; the main -- to be ran from the command line, takes in a filename, tests satisfiability [and writes results to output file]
(define (main) : Void
  (define args (vector->list (current-command-line-arguments)))
  (define n (length args))
  (if (not (or (equal? n 1) (equal? n 2)))
      (printf "Usage: racket read_cnf <in_filename> [<out_filename>]")
      (begin
        (let-values ([(cnf num-vars) (parse-file (first args))])
          (define result (interp cnf -1 num-vars #f))
          (if (equal? result -1)
              (printf "UNSAT\n")
              (begin
                (printf "SAT\n")
                (printf "~a0\n" (get-sat-result (build-list num-vars add1) (bin-to-bool (padded-binary result num-vars))))))))))

; will parse the input cnf file
(define (parse-file [file : String]) : (Values (Listof (Listof Integer)) Integer)
  (define in-port (open-input-file file))
  (define result-int : Integer 0)
  (define result-list : (Listof (Listof Integer)) '())
  (for ([line (in-lines in-port)])
    (cond
      [(equal? (string-ref line 0) #\p) (set! result-int (first (string->int-list line)))]
      [(starts-with-int? line) (set! result-list (cons (string->int-list line) result-list))]))
  (close-input-port in-port)
  (values (reverse result-list) result-int))

; checks if the first character in a string is an integer
(define (starts-with-int? [line : String]) : Boolean
  (cond
    [(equal? line "") #f]
    [(char-numeric? (string-ref line 0)) #t]
    [(and (equal? (string-ref line 0) #\-) (char-numeric? (string-ref line 1))) #t]
    [else #f]))

; converts a string to a list of integers
(define (string->int-list [s : String]) : (Listof Integer)
  (filter (lambda (x) (not (equal? x 0)))
          (filter exact-integer?
                  (map string->number (string-split s)))))

(main)