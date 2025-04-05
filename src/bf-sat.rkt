#lang typed/racket

(require "sat-util.rkt")
(require "read-cnf.rkt")

; -------------------------
; ---- Brute-Force-SAT ----
; -------------------------

; kb = knowledge base (∆)
(define (bf-sat [kb : (Listof (Listof Integer))] [num-vars : Integer]) : (U Boolean (Listof Integer))
  (let* ([vars (build-list num-vars add1)]
         [result (interp kb -1 num-vars #f)])
    (if (equal? result -1)
        #f
        (for/list : (Listof Integer)
          ([bool (in-list (bin-to-bool (padded-binary result num-vars)))]
           [var (in-list vars)])
          (if bool var (- var))))))

; Evaluates a single formula given a truth value assignment
(define (eval [form : (Listof Integer)] [tvas : (Listof Boolean)]) : Boolean
  (define (eval-help [form : (Listof Integer)] [tvas : (Listof Boolean)]) : (Listof Boolean)
    (match form
      ['() '()]
      [(cons f r) (if (positive? f)
                      (cons (list-ref tvas (- f 1)) (eval-help r tvas))
                      (cons (not (list-ref tvas (- (abs f) 1))) (eval-help r tvas)))]))
  (ormap (lambda ([b : Boolean]) b) (eval-help form tvas)))

; Given formulas, returns the valid truth value assignment
(define (interp [forms : (Listof (Listof Integer))] [tvas : Integer] [num-vars : Integer] [sat : Boolean]) : Integer
  (match sat
    [#t tvas]
    [#f (if (>= (+ 1 tvas) (expt 2 num-vars)) ; if all tvas have been checked
            -1 ; return -1 to represent no tva
            (let ([lst-tvas (bin-to-bool (padded-binary (+ 1 tvas) num-vars))])
              (interp forms (+ 1 tvas) num-vars (interp-help forms lst-tvas))))]))

; Recurses over the list of formulas, calling eval on them
(define (interp-help [forms : (Listof (Listof Integer))] [tvas : (Listof Boolean)]) : Boolean
  (define (help [forms : (Listof (Listof Integer))] [tvas : (Listof Boolean)]) : (Listof Boolean)
    (match forms
      ['() '()]
      [(cons f r) (cons (eval f tvas) (help r tvas))]))
  (andmap (lambda ([b : Boolean]) b) (help forms tvas)))

; takes an integer, converts it to binary string with given padding
(define (padded-binary [num : Integer] [width : Integer]) : String
  (define bin (number->string num 2))
  (string-append (make-string (- width (string-length bin)) #\0) bin))

; takes a binary string an converts it to a list of booleans
(define (bin-to-bool [s : String]) : (Listof Boolean)
  (map (lambda ([c : Char]) (equal? c #\1)) (string->list s)))

; allows .cnf file input upon run
(define (main) : Void
  (define args (vector->list (current-command-line-arguments)))
  (define n (length args))
  (if (or (> n 2) (< n 1))
      (printf "Usage: ./bf-sat <in_filename> [<out_filename>]\n")
      (let-values ([(cnf num-vars) (parse-file (first args))])
        (define result (bf-sat cnf num-vars))
        (define (output-fun) : Void
          (match result
            [#f (printf "UNSAT\n")]
            [(list p ...)
             (printf "SAT\n")
             (printf "~a0\n"
                     (tva->string (sort (apply list p) (lambda ([x : Integer] [y : Integer]) (< (abs x) (abs y))))))]))
        (if (= n 2)
            (with-output-to-file (list-ref args 1) output-fun #:exists 'replace)
            (output-fun)))))

(main)