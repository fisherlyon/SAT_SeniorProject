#lang typed/racket

(require "sat-util.rkt")
(require "read-cnf.rkt")

; ------------------
; ---- CDCL-SAT ----
; ------------------

; An Implication Graph Node
;; (struct decNode ; decision node
;;   ([lit : Integer] ; decided literal
;;    [level : Integer] ; level at which decision is made
;;    [branches : (Listof (U implNode contrNode))])) ; out-going branches
;; (struct implNode ; implication node
;;   ([lit : Integer] ; implied literal
;;    [level : Integer] ; level at which implication is made
;;    [kb-cl : Integer] ; knowledge base clause the implication was derived from
;;    [branches : (Listof (U implNode contrNode))])) ; out-going branches
;; (struct contrNode ; contradiction node
;;   ([kb-cl : Integer])) ; knowledge base clause the contradiction was derived from

; An Implication Graph Node
(struct IG-Node
  ([lit : Integer] ; corresponding literal
   [level : Integer] ; decision level
   [reason : (Option Integer)] ; clause ID that caused this implication, or #f for decisions
   [index : Integer]) ; propagation order (for walking back through the graph)
  #:transparent)

; kb = knowledge base (∆)
; D = a decision sequence
; G = a set of learned clauses
(define (cdcl-sat [kb : (Listof (Listof Integer))]) : Boolean
  (define D : (Boxof (Listof Integer)) (box '()))
  (define G : (Listof (Listof Integer)) '())
  (define I : (Listof Integer) '())
  (define (while-true) : Boolean
    (let-values ([(I temp-kb) (cdcl-unit-res kb G (unbox D) I)])
      (printf "\ntemp-kb = ~a\nI = ~a\nD = ~a\n\n" temp-kb I (unbox D))
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
                #t)))))
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
       G
       '()
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
    (set! index (add1 index)) ; increment index
    (hash-set! nodes (list-ref I i) ; add implication node
               (find-impl-clause kb (map - temp) (list-ref I i) 0 index nodes))
    (set! temp (append temp (list (list-ref I i))))) ; update temp (D U processed I)
  ; add the last contradiction node
  (hash-set! nodes 0 (find-impl-clause kb (map - temp) 0 0 (add1 index) nodes))
  (for ([(key value) (in-hash nodes)])
    (printf "~a: ~a\n" key value))
  (define asserting (reverse (get-asserting (hash-keys nodes) nodes clauses)))
  (printf "~a\n" asserting)
  (if (equal? (length asserting) 1)
      (values (car (first asserting)) (cdr (first asserting)))
      (values '() 0)))

; Returns the asserting clauses and assertion levels
; They are the learnt clauses that include only one variable setting
; at the last decision level
(define (get-asserting [keys : (Listof Integer)]
                            [nodes : (HashTable Integer IG-Node)]
                            [clauses : (HashTable Integer (Listof Integer))])
  : (Listof (Pairof (Listof Integer) Integer))
  (match keys
    ['() '()]
    [(cons f r)
     (cond
       ; contradiction case
       [(equal? f 0)
        (let* ([clause (cast (hash-ref clauses (IG-Node-reason (hash-ref nodes f))) (Listof Integer))]
               [levels (map (lambda ([cl : Integer]) (IG-Node-level (hash-ref nodes (- cl)))) clause)])
          (if (not (has-duplicates? levels))
              (cons (cons clause (get-assertion-level levels)) (get-asserting r nodes clauses))
              (get-asserting r nodes clauses)))]
       ; implication node case
       [(not (equal? (IG-Node-reason (hash-ref nodes f)) #f)) 
        (let* ([clause (set-diff (cast (hash-ref clauses (IG-Node-reason (hash-ref nodes f))) (Listof Integer)) (list f))]
               [levels (map (lambda ([cl : Integer]) (IG-Node-level (hash-ref nodes (- cl)))) clause)])
          (if (not (has-duplicates? levels))
              (cons (cons clause (get-assertion-level levels)) (get-asserting r nodes clauses))
              (get-asserting r nodes clauses)))]
       ; decision node case
       [else (get-asserting r nodes clauses)])]))

; Gets the assertion level of an asserting clause
(define (get-assertion-level [levels : (Listof Integer)]) : Integer
  (if (equal? (length levels) 1)
      -1
      (apply min levels)))

; Gets the information for an implication and returns the IG-Node
(define (find-impl-clause [kb : (Listof (Listof Integer))] ; knowledge base
                          [D : (Listof Integer)] ; decision sequence
                          [I : Integer] ; single implication
                          [reason : Integer]
                          [index : Integer]
                          [nodes : (HashTable Integer IG-Node)]) ; the depth in the graph
  : IG-Node
  (printf "fic: kb = ~a\n" kb)
  (match kb
    ['() (error "No implication found")] ; Handle case when no clause matches
    [(cons f r)
     (if (equal? I 0) ; Contradiction stage
         (if (implication? f D)
             (make-ig-node I (get-level nodes f) reason index)
             (find-impl-clause r D 0 (add1 reason) index nodes))
         (if (and (not (equal? (member I f) #f)) (implication? f (append D (list I))))
             (make-ig-node I (get-level nodes (set-diff f (list I))) reason index)
             (find-impl-clause r D I (add1 reason) index nodes)))]))

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
                      [reason : (Option Integer)] [index : Integer]) : IG-Node
  (IG-Node val level reason index))

; Set Difference Function
(define (set-diff [A : (Listof Integer)] [B : (Listof Integer)])
  (filter (lambda (x) (not (member x B))) A))

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


; Turns a list of integers into a list of unit (integer) clauses
(define (ints->clauses [unit-clauses : (Listof Integer)]) : (Listof (Listof Integer))
  (match unit-clauses
    ['() '()]
    [(cons f r) (cons (list f) (ints->clauses r))]))

; Erase decisions after the given assertion level (m)
(define (erase [m : Integer] [D : (Boxof (Listof Integer))]) : Void
  (if (equal? m -1)
      (set-box! D '())
      (set-box! D (take (unbox D) (min (length (unbox D)) (add1 m))))))

; Adds an element to a mutable list (Boxof (Listof ...))
(define (add [l : Integer] [D : (Boxof (Listof Integer))]) : Void
  (set-box! D (append (unbox D) (list l))))