#lang typed/racket

(require "sat-util.rkt")
(require "read-cnf.rkt")

; ------------------
; ---- CDCL-SAT ----
; ------------------

; An Implication Graph Node
(struct IG-Node
  ([lit : Integer] ; corresponding literal
   [level : Integer] ; decision level
   [cause : (Option Integer)] ; clause ID that caused this implication, or #f for decisions
   [index : Integer]) ; propagation order (for walking back through the graph)
  #:transparent)

; kb = knowledge base (∆)
; D = a decision sequence
; G = a set of learned clauses
(define (cdcl-sat [kb : (Listof (Listof Integer))]) : (U Boolean (Listof Integer))
  (define D : (Boxof (Listof Integer)) (box '()))
  (define G : (Listof (Listof Integer)) '())
  (define I : (Listof Integer) '())
  (define (while-true) : (U Boolean (Listof Integer))
    (let-values ([(I temp-kb) (cdcl-unit-res kb G (unbox D) I)])
      (printf "\nI = ~a\ntemp-kb = ~a\nD = ~a\nG = ~a\n" I temp-kb D G)
      (if (equal? temp-kb '{{}}) ; if unit resolution detects a contradiction
          (if (equal? (unbox D) '()) ; contradiction without any decisions
              #f
              (let-values ([(a m) (impl-graph (append kb G) (unbox D) I)])
                (erase m D)
                (set! G (append G (list a)))
                (while-true)))
          (let ([l (next-decision temp-kb I)]) ; get the next decision
            ; if l is a literal where neither l or ¬l are implied by unit resolution
            (if (not (equal? l 0))
                (begin
                  (add l D) ; add new decision to sequence D
                  (while-true)) ; loop
                I))))) ; 
  (while-true))

; Unit Resolution for CDCL
(define (cdcl-unit-res [kb : (Listof (Listof Integer))] ; knowledge base
                       [G : (Listof (Listof Integer))] ; set of learned clauses
                       [D : (Listof Integer)] ; decision sequence
                       [I : (Listof Integer)]) ; implications based on unit resolution
  : (Values (Listof Integer) (Listof (Listof Integer)))
  (define temp (append kb G (ints->clauses D)))
  (define unit-clause (find-unit-clause temp))
  (if (equal? unit-clause '{})
      (values (reverse I) kb)
      (cdcl-unit-res
       (condition temp (first unit-clause))
       '() ; added during first iteration
       '() ; added during first iteration
       (cons (first unit-clause) I))))

; Builds an implication graph
(define (impl-graph [kb : (Listof (Listof Integer))] ; knowledge base
                          [D : (Listof Integer)] ; decision sequence
                          [I : (Listof Integer)]) ; implication sequence from unit res
  : (Values (Listof Integer) Integer)
  (set! I (set-diff I D)) ; implications - decisions
  (define temp D)
  (define index 0)
  (define nodes : (HashTable Integer IG-Node) (make-hash))
  (define clauses : (HashTable Integer (Listof Integer)) (make-hash))
  (for ([i (in-range (length D))])
    (hash-set! nodes (list-ref D i) (make-ig-node (list-ref D i) i #f index))) ; init decisions 
  (for ([i (in-range (length kb))]) ; init clauses
    (hash-set! clauses i (list-ref kb i)))
  (for ([i (in-range (length I))])
    (printf "\nkb = ~a\nD = ~a\nI = ~a\n" kb D I)
    (for ([(key value) (in-hash nodes)])
      (printf "~a: ~a\n" key value))
    (set! index (add1 index)) ; increment index
    (hash-set! nodes (list-ref I i) ; add implication node
               (find-impl-clause kb (map - temp) (list-ref I i) 0 index nodes))
    (set! temp (append temp (list (list-ref I i))))) ; update temp (D U processed I)
  ; add the last contradiction node
  (hash-set! nodes 0 (find-impl-clause kb (map - temp) 0 0 (add1 index) nodes))
  (define asserting (get-asserting (hash-keys nodes) nodes clauses (- (length D) 1)))
  (printf "\nasserting clause = ~a\n" asserting)
  (if (not (equal? (car asserting) '()))
      (values (car asserting) (cdr asserting))
      (error 'impl-graph "Error: Couldn't find an asserting clause.")))


; Returns the asserting clauses and assertion levels
; They are the learnt clauses that include only one variable setting
; at the last decision level
(define (get-asserting [keys : (Listof Integer)]
                       [nodes : (HashTable Integer IG-Node)]
                       [clauses : (HashTable Integer (Listof Integer))]
                       [max-dl : Integer])
  : (Pairof (Listof Integer) Integer)
  (define (helper [keys : (Listof Integer)] 
                  [best : (Pairof (Listof Integer) Integer)]
                  [best-index : Integer])
    : (Pairof (Listof Integer) Integer)
    (printf "best = ~a\n" best)
    (match keys
      ['() best]
      [(cons f r)
       (cond
         ; contradiction case
         [(equal? f 0)
          (let* ([clause (cast (hash-ref clauses (IG-Node-cause (hash-ref nodes f))) (Listof Integer))]
                 [levels (map (lambda ([cl : Integer]) (IG-Node-level (hash-ref nodes (- cl)))) clause)]
                 [node-index (IG-Node-index (hash-ref nodes f))])
            (if (and (not (has-duplicates? levels)) (> node-index best-index))
                (helper r 
                       (cons clause (get-assertion-level levels))
                       node-index)
                (helper r best best-index)))]
         ; implication node case
         [(not (equal? (IG-Node-cause (hash-ref nodes f)) #f)) 
          (let* ([clause (set-diff (cast (hash-ref clauses (IG-Node-cause (hash-ref nodes f))) (Listof Integer)) (list f))]
                 [levels (map (lambda ([cl : Integer]) (IG-Node-level (hash-ref nodes (- cl)))) clause)]
                 [node-index (IG-Node-index (hash-ref nodes f))])
            (printf "\nclause = ~a\nlevels = ~a\nindex = ~a\n" clause levels node-index)
            (printf "\n~a - ~a\n" (cast (hash-ref clauses (IG-Node-cause (hash-ref nodes f))) (Listof Integer)) (list f))
            (if (and (not (has-duplicates? levels)) (> node-index best-index))
                (if (equal? clause '())
                    (helper r best best-index) ;; TO-DO
                    (helper r 
                       (cons clause (get-assertion-level levels))
                       node-index))
                (helper r best best-index)))]
         ; decision node case
         [else (helper r best best-index)])]))
  
  ; initial call with empty best clause and -1 index
  (helper keys (cons '() 0) -1))

; Gets the assertion level of an asserting clause
(define (get-assertion-level [levels : (Listof Integer)]) : Integer
  (if (equal? (length levels) 1)
      -1
      (apply min levels)))

; Gets the information for an implication and returns the IG-Node
(define (find-impl-clause [kb : (Listof (Listof Integer))] ; knowledge base
                          [D : (Listof Integer)] ; decision sequence
                          [I : Integer] ; single implication
                          [cause : Integer]
                          [index : Integer]
                          [nodes : (HashTable Integer IG-Node)]) ; the depth in the graph
  : IG-Node
  (match kb
    ['() (error "No implication found")] ; Handle case when no clause matches
    [(cons f r)
     (cond
       ; Unit clause implication case
       [(and (equal? (length f) 1) (equal? (first f) I))
        (make-ig-node I 
                      (IG-Node-level (hash-ref nodes (- (first D)))) ; level of last decision
                      cause 
                      index)]
       ; Contradiction stage
       [(equal? I 0) 
        (if (implication? f D)
            (make-ig-node I (get-level nodes f) cause index)
            (find-impl-clause r D 0 (add1 cause) index nodes))]
       ; Regular implication stage
       [(and (not (equal? (member I f) #f)) 
             (implication? f (append D (list I))))
        (make-ig-node I (get-level nodes (set-diff f (list I))) cause index)]
       ; No match found, continue searching
       [else (find-impl-clause r D I (add1 cause) index nodes)])]))

; Checks if a given clause leads to an implication
(define (implication? [clause : (Listof Integer)] [D+I : (Listof Integer)]) : Boolean
  (match clause
    ['() #t]
    [(cons f r)
     (cond
       [(equal? (member f D+I) #f) #f]
       [else (implication? r D+I)])]))

; Gets the level of an implication given its incoming branches
(define (get-level [nodes : (HashTable Integer IG-Node)] [clause : (Listof Integer)]) : Integer
  (apply max
         (map
          (lambda ([d : Integer])
            (IG-Node-level (cast (hash-ref nodes (- d)) IG-Node))) clause)))

; Makes an Implication Graph Node
(define (make-ig-node [val : Integer] [level : Integer]
                      [cause : (Option Integer)] [index : Integer]) : IG-Node
  (IG-Node val level cause index))

; Checks if a list has duplicates or not
(define (has-duplicates? [lst : (Listof Any)])
  (not (equal? (length lst) (length (remove-duplicates lst)))))

; Selects a literal where neither l or ¬l are implied by unit resolution
; kb : knowledge base
; I : implications from unit resolution
(define (next-decision [kb : (Listof (Listof Integer))] [I : (Listof Integer)]) : Integer
  (define decisions : (Listof Integer) '())
  (for ([clause kb])
    (set! decisions (append decisions (get-uniq-vars clause I))))
  (if (equal? decisions '())
      0
      (first (remove-duplicates decisions))))

; Helper for getting the next decision, returns a list of possibilities per clause in kb
(define (get-uniq-vars [clause : (Listof Integer)] [I : (Listof Integer)]) : (Listof Integer)
  (match clause
    ['() '()]
    [(cons f r)
     (let* ([pos-res (member f I)]
            [neg-res (member (- f) I)])
      (cond
       [(and (equal? pos-res #f) (equal? neg-res #f)) (cons f (get-uniq-vars r I))]
       [else (get-uniq-vars r I)]))]))

; allows .cnf file input upon run
(define (main) : Void
  (define args (vector->list (current-command-line-arguments)))
  (define n (length args))
  (if (or (> n 2) (< n 1))
      (printf "Usage: ./cdcl-sat <in_filename> [<out_filename>]\n")
      (let-values ([(cnf num-vars) (parse-file (first args))])
        (define result (cdcl-sat cnf))
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