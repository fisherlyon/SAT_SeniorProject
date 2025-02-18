#lang typed/racket

(require "sat-util.rkt")

; ----------------
; ---- SAT-II ----
; ----------------

; Returns a partial variable instantiation since we are not traversing
; the tree all the way to the leaves.
; To get a TVA, it isn't always necessary to traverse the tree all the way to a leaf.

; kb = knowledge base (∆)
; d = depth (initially set to -1)
; P = given variables; assuming P_1, P_2, ..., P_n
; L = the accumulated partial variable instantiation
(define (sat2 [kb : (Listof (Listof Integer))] [d : Integer] [P : (Listof Integer)] [L : (Listof Integer)]) : (U Boolean (Listof Integer))
  (cond
    [(equal? kb '{}) (reverse L)]
    [(equal? kb '{{}}) #f]
    [else
     (let ([pos-result (sat2 (condition kb (list-ref P (+ d 1))) (+ d 1) P (cons (list-ref P (+ d 1)) L))])
       (if (not (equal? pos-result #f))
           pos-result
           (sat2 (condition kb (- (list-ref P (+ d 1)))) (+ d 1) P (cons (- (list-ref P (+ d 1))) L))))]))