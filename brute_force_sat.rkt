#lang typed/racket

(require typed/rackunit)
(require "tseitin.rkt") ; adding usage of Formulas and tseitin

; brute force SAT solver for a given sexp formula -- work in progress
(define (bf-sat [form : Sexp]) : (Listof (Pairof Symbol Boolean))
  (let* ([rf-form (reformat (tseitin form))] ; reformatted cnf form -- (()())
         [vars (extract-vars rf-form '())] ; the extracted sorted variables from the above cnf
         [num-subs (build-list (length vars) add1)] ; the number substitutes for the above variables
         [num-form (subst-nums-for-vars rf-form vars num-subs)] ; the number form cnf of the reformatted cnf
         [result (interp num-form -1 (length vars) #f)]) ; the result of the brute force sat test
    (if (equal? result -1)
        '((NoTVA . #f))
        (get-result vars (bin-to-bool (padded-binary result (length vars)))))))

; gathers the result
(define (get-result [vars : (Listof Symbol)] [tva : (Listof Boolean)]) : (Listof (Pairof Symbol Boolean))
  (match (list vars tva)
    [(list '() '()) '()]
    [(list (cons f1 r1) (cons f2 r2)) (cons (cons f1 f2) (get-result r1 r2))]))

; evaluates a single formula given a truth value assignment
(define (eval [form : (Listof Integer)] [tvas : (Listof Boolean)]) : Boolean
  (define (eval-help [form : (Listof Integer)] [tvas : (Listof Boolean)]) : (Listof Boolean)
    (match form
      ['() '()]
      [(cons f r) (if (positive? f)
                      (cons (list-ref tvas (- f 1)) (eval-help r tvas))
                      (cons (not (list-ref tvas (- (abs f) 1))) (eval-help r tvas)))]))
  (ormap (lambda ([b : Boolean]) b) (eval-help form tvas)))

; given formulas, returns the valid truth value assignment
(define (interp [forms : (Listof (Listof Integer))] [tvas : Integer] [num-vars : Integer] [sat : Boolean]) : Integer
  (match sat
    [#t tvas]
    [#f (if (>= (+ 1 tvas) (expt 2 num-vars)) ; if all tvas have been checked
            -1 ; return -1 to represent no tva
            (let ([lst-tvas (bin-to-bool (padded-binary (+ 1 tvas) num-vars))])
              (interp forms (+ 1 tvas) num-vars (interp-help forms lst-tvas))))]))

; recurses over the list of formulas, calling eval on them
(define (interp-help [forms : (Listof (Listof Integer))] [tvas : (Listof Boolean)]) : Boolean
  (define (help [forms : (Listof (Listof Integer))] [tvas : (Listof Boolean)]) : (Listof Boolean)
    (match forms
      ['() '()]
      [(cons f r) (cons (eval f tvas) (help r tvas))]))
  (andmap (lambda ([b : Boolean]) b) (help forms tvas)))

; extract variables from cnf and sort them
(define (extract-vars [cnf : (Listof (Listof Formula))] [result : (Listof Symbol)]) : (Listof Symbol)
  (match cnf
    ['() (sort result symbol<?)]
    [(cons f r)
     (extract-vars
      r
      (foldl (lambda ([form : Formula] [acc : (Listof Symbol)])
               (match form
                 [(varF var) (if (member var acc) acc (cons var acc))]
                 [(auxF var) (if (member var acc) acc (cons var acc))]
                 [(notF (varF var)) (if (member var acc) acc (cons var acc))]
                 [(notF (auxF var)) (if (member var acc) acc (cons var acc))]
                 [_ (error 'extract-vars "invalid cnf, given ~e" form)]))
             result
             f))]))

; sub in numbers for variables in the reformatted cnf
(define (subst-nums-for-vars [forms : (Listof (Listof Formula))] [vars : (Listof Symbol)] [nums : (Listof Integer)]) : (Listof (Listof Integer))
  (match forms
    ['() '()]
    [(cons f r) (cons (single-form-subst f vars nums) (subst-nums-for-vars r vars nums))]))

; subs in a list of numbers for a list of variables in a formula
(define (single-form-subst [form : (Listof Formula)] [vars : (Listof Symbol)] [nums : (Listof Integer)]) : (Listof Integer)
  (match (list vars nums)
    [(list '() '()) '()]
    [(list (cons f1 r1) (cons f2 r2)) (append (single-num-var-subst form f1 f2) (single-form-subst form r1 r2))]))

; helper for subst numbers -- subs in a number for a variable in a formula
(define (single-num-var-subst [form : (Listof Formula)] [var : Symbol] [num : Integer]) : (Listof Integer)
  (match form
    ['() '()]
    [(cons f r)
     (match f
       [(varF v) (if (equal? v var)
                     (cons num (single-num-var-subst r var num))
                     (single-num-var-subst r var num))]
       [(auxF v) (if (equal? v var)
                     (cons num (single-num-var-subst r var num))
                     (single-num-var-subst r var num))]
       [(notF (varF v)) (if (equal? v var)
                            (cons (- num) (single-num-var-subst r var num))
                            (single-num-var-subst r var num))]
       [(notF (auxF v)) (if (equal? v var)
                            (cons (- num) (single-num-var-subst r var num))
                            (single-num-var-subst r var num))]
       [_ (error 'single-num-var-subst "invalid var, given ~e" f)])]))

; reformat cnf to list of list of formulas
(define (reformat [form : Formula]) : (Listof (Listof Formula))
  (match form
    [(andF forms)
     (foldl (lambda ([f : Formula] [acc : (Listof (Listof Formula))])
              (match f
                [(auxF var) (cons (list f) acc)]
                [(orF frs) (cons frs acc)]
                [_ (error 'reformat "invalid cnf, given ~e" f)]))
            '()
            forms)]))

; takes an integer, converts it to binary string with given padding
(define (padded-binary [num : Integer] [width : Integer]) : String
  (define bin (number->string num 2))
  (string-append (make-string (- width (string-length bin)) #\0) bin))

; takes a binary string an converts it to a list of booleans
(define (bin-to-bool [s : String]) : (Listof Boolean)
  (map (lambda ([c : Char]) (equal? c #\1)) (string->list s)))

; tests
; extract-vars tests
(check-equal?
 (extract-vars (reformat (tseitin '(-> (& (v p q) r) (! s)))) '())
 '(p q r s x1 x2 x3 x4))

(check-equal?
 (extract-vars (reformat (tseitin '(v (& A (! B) C) (-> D (<-> E (! F))) G))) '())
 '(A B C D E F G x1 x2 x3 x4 x5 x6))

; reformat tests
(check-equal?
 (reformat (tseitin '(-> (& (v p q) r) (! s))))
 (list
  (list (auxF 'x4) (varF 's))
  (list (notF (varF 's)) (notF (auxF 'x4)))
  (list (auxF 'x3) (notF (varF 'q)))
  (list (auxF 'x3) (notF (varF 'p)))
  (list (varF 'q) (varF 'p) (notF (auxF 'x3)))
  (list (auxF 'x2) (notF (varF 'r)) (notF (auxF 'x3)))
  (list (varF 'r) (notF (auxF 'x2)))
  (list (auxF 'x3) (notF (auxF 'x2)))
  (list (auxF 'x1) (notF (auxF 'x4)))
  (list (auxF 'x1) (auxF 'x2))
  (list (auxF 'x4) (notF (auxF 'x2)) (notF (auxF 'x1)))
  (list (auxF 'x1))))