#lang typed/racket

#|
The core purpose of this file is to take in a DIMACS .cnf file and parse it.
The parsed result is the knowledge base in clausal form and the number of variables.
|#

(provide (all-defined-out))
;(require typed/rackunit)
;(require "tseitin.rkt")
;(require "brute_force_sat.rkt")

; the main -- to be ran from the command line, takes in a filename, tests satisfiability [and writes results to output file]
;; (define (main) : Void
;;   (define args (vector->list (current-command-line-arguments)))
;;   (define n (length args))
;;   (if (not (or (equal? n 1) (equal? n 2)))
;;       (printf "Usage: racket read_cnf <in_filename> [<out_filename>]")
;;       (begin
;;         (let-values ([(cnf num-vars) (parse-file (first args))])
;;           (define result (interp cnf -1 num-vars #f))
;;           (if (equal? result -1)
;;               (printf "UNSAT\n")
;;               (begin
;;                 (printf "SAT\n")
;;                 (printf "~a0\n" (get-sat-result (build-list num-vars add1) (bin-to-bool (padded-binary result num-vars))))))))))

; will parse the input cnf file, returning the cnf list and the number of variables
(define (parse-file [file : String]) : (Values (Listof (Listof Integer)) Integer)
  (define in-port (open-input-file file))
  (define num-vars : Integer 0)
  (define cnf-list : (Listof (Listof Integer)) '())
  (for ([line (in-lines in-port)])
    (define line-list (string-split line))
    (match line-list
      ['() (void)] ; ignore empty lines
      [(list "p" "cnf" vars clauses) ; problem/header line
       (if (int? vars)
           (set! num-vars (str->int vars))
           (error 'parse-file "Invalid 'p' line in CNF file, given ~a" line))]
      [(list "c" text ...) (void)] ; ignore comment lines
      [(list nums ...)
       (if (and (int-list? line-list) (> (length line-list) 1))
           (set! cnf-list (cons (str-list->int-list line-list) cnf-list))
           (void))]
      [_ (void)]))
  (close-input-port in-port)
  (values (reverse cnf-list) num-vars))

; checks if a string is an integer?
(define (int? [s : String]) : Boolean
  (integer? (string->number s)))

; checks if each string in a list is an integer
(define (int-list? [strs : (Listof String)]) : Boolean
  (match strs
    ['() #t]
    [(cons f r) (if (int? f) (int-list? r) #f)]))

; converts a string to an integer
(define (str->int [s : String]) : Integer
  (cast (string->number s) Integer))

; turns a list of strings to a list of integers
(define (str-list->int-list [l : (Listof String)]) : (Listof Integer)
  (match l
    ['() '()]
    [(cons f r)
     (if (int? f)
         (if (equal? f "0") (str-list->int-list r) (cons (str->int f) (str-list->int-list r)))
         (error 'str-list->int-list "invalid line in cnf file, given ~a" (string-join l)))]))

;;(main)