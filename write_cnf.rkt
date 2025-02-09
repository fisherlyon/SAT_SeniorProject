#lang typed/racket

(require typed/rackunit)
(require "tseitin.rkt") ; adding usage of Formulas and tseitin
(require "brute_force_sat.rkt")

; the main -- to be ran from the command line, takes in a filename and a formula, returns the cnf file
(define (main) : Void
  (define args (vector->list (current-command-line-arguments)))
  (if (not (equal? (length args) 2))
      (printf "Usage: racket write_cnf.rkt <filename> \"<formula>\"\n")
      (write-cnf-file
       (first args)
       (cast (read (open-input-string (second args))) Sexp))))

; given a formula, convert it to CNF and write it to a file in .cnf format
(define (write-cnf-file [filename : String] [form : Sexp]) : Void
  (let* ([rf-form (reformat (tseitin form))] ; reformatted cnf form -- (()())
         [vars (extract-vars rf-form '())] ; the extracted sorted variables from the above cnf
         [num-subs (build-list (length vars) add1)] ; the number substitutes for the above variables
         [num-form (subst-nums-for-vars rf-form vars num-subs)])
    (with-output-to-file filename
      (lambda ()
        (printf "c FILE: ~a\nc\n" filename)
        (printf "p cnf ~a ~a\n" (length vars) (length num-form))
        (for ([disjunct num-form])
          (printf "~a 0\n" (string-join (map ~a disjunct) " "))))
      #:mode 'text
      #:exists 'replace)))

; retrieves the name of a file from the user
(define (get-filename) : String
  (display "Enter the CNF filename: ")
  (flush-output) ; ensure prompt is displayed before input
  (define input (read-line))
  (match input
    [(? string?) (if (equal? input "") "default.cnf" input)]
    [_ (error 'get-filename "invalid filename")]))

(main)