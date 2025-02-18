#lang typed/racket

(require "sat-util.rkt")

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