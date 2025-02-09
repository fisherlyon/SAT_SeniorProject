#lang typed/racket

#|
<descritption here>
|#

(require typed/rackunit)
(require "tseitin.rkt")
(require "brute_force_sat.rkt")


; the main -- to be ran from the command line, takes in a filename, tests satisfiability [and writes results to output file]
(define (main) : Void
  (define args (vector->list (current-command-line-arguments)))
  (if (not (or (equal? (length args) 1) (equal? (length args) 2)))
      (printf "Usage: racket read_cnf <in_filename> [<out_filename>]")
      ))