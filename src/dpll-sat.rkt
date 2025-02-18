#lang typed/racket

; ------------------
; ---- DPLL-SAT ----
; ------------------

; kb = knowledge base (∆)
(define (dpll-sat [kb : (Listof (Listof Integer))]) : (U Boolean (Listof Integer))
  #t)


