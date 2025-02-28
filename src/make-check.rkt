#lang typed/racket

; given SAT result, append the TVA to the end of a .cnf file as unit clauses for testing SAT correctness
(define (write-check [filename : String] [tva : String]) : Void
  (with-output-to-file filename
    (lambda ()
      (printf "c\nc Begin Unit Clause TVA Check\nc\n")
      (for ([u (string-split tva)])
        (if (not (equal? u "0"))
            (printf "~a 0\n" u)
            (void)))
      (printf "c\nc End Unit Clause TVA Check\nc\n"))
    #:mode 'text
    #:exists 'append))

(define (main) : Void
  (define args (vector->list (current-command-line-arguments)))
  (define n (length args))
  (if (not (equal? n 2))
      (printf "Usage: ./make-check <SAT_OUT> <check_filename>\n")
      (let* ([sat-out (first args)]
             [check-filename (second args)]
             [lines (with-input-from-file sat-out
                      (lambda ()
                        (port->lines (current-input-port))))])
        (if (< (length lines) 2)
            (printf "Error: SAT_OUT file does not have enough lines.\n")
            (write-check check-filename (list-ref lines 1))))))

(main)