#lang typed/racket

(require "sat-util.rkt")
(require "read-cnf.rkt")

; ------------------
; ---- CDCL-SAT ----
; ------------------

; Node structure for Implication Graph
(struct IG-Node
  ([lit : Integer]            ; Decision/Implication Literal
   [declev : Integer]         ; Decision Level
   [cause : (Option Integer)] ; Clause in knowledge base that caused the implication (#f for decisions)
   [depth : Integer])         ; # of edges from a decision to the current Node
  #:transparent)

; Implication Graph structure: Nodes defined above and Edges (adjacency list)
(struct ImplGraph
  ([nodes : (HashTable Integer IG-Node)]              ; Vertices (literals -> nodes)
   [incoming : (HashTable Integer (Listof Integer))]  ; Incoming Edges
   [outgoing : (HashTable Integer (Listof Integer))]) ; Outgoing Edges
  #:transparent)

; Makes an Implication Graph
(define (make-impl-graph): ImplGraph
  (ImplGraph (make-hash) (make-hash) (make-hash)))

; Adds a decision node to the implication graph (node without incoming edges)
(: add-dec! (-> ImplGraph Integer Integer Integer Void))
(define (add-dec! graph lit declev depth)
  (hash-set! (ImplGraph-nodes graph) lit (IG-Node lit declev #f depth))
  (hash-set! (ImplGraph-incoming graph) lit '())
  (hash-set! (ImplGraph-outgoing graph) lit '()))

; Adds an implication node to the implication graph (node derived from decisions)
(: add-impl! (-> ImplGraph Integer Integer Integer Integer (Listof Integer) Void))
(define (add-impl! graph lit declev cause depth nodes-from)
  (hash-set! (ImplGraph-nodes graph) lit (IG-Node lit declev cause depth))
  (hash-set! (ImplGraph-incoming graph) lit nodes-from)
  (hash-set! (ImplGraph-outgoing graph) lit '())

  (for ([node (in-list nodes-from)]) ; for each node connected to the new impl node
    (let ([outgoing-list (hash-ref (ImplGraph-outgoing graph) node)]) ; get the current node's edge list
      (hash-set! (ImplGraph-outgoing graph) node (cons lit outgoing-list))))) ; update the edge lis)

; CDCL SAT
; kb = knowledge base (∆)
; D = a decision sequence (mutable)
; G = a set of learned clauses
; I = a set of literals that were either present as unit clauses in kb (∆)
;     or derived  by unit resolution
(define (cdcl-sat [kb : (Listof (Listof Integer))]) : (U Boolean (Listof Integer))
  (define D : (Boxof (Listof Integer)) (box '()))
  (define G : (Listof (Listof Integer)) '{})
  (define I : (Listof Integer) '())
  (define (while-true) : (U Boolean (Listof Integer))
    (let-values ([(I new-kb) (cdcl-unit-res kb G (unbox D) I)])
      (if (equal? new-kb '{{}}) ; if unit resolution detects a contradiction
          (if (empty? (unbox D)) ; contradiction without any decisions
              #f ; so return false -- UNSAT
              (let* ([graph (build-impl-graph (append kb G) (unbox D) I)] ; build the implication graph
                     [asserting (get-asserting graph)] ;  get the asserting info
                     [a (car asserting)] ; get asserting clause
                     [m (cdr asserting)]) ; get assertion level
                (erase m D) ; erase decisons past the resulting assertion level
                (set! G (append G (list a))) ; add the learned/asserting clause to G
                (while-true)))
          (let ([l (if (empty? new-kb) 0 (first (first new-kb)))]) ; get the next decision
            ; if l is a literal where neither l or ¬l are implied by unit resolution
            (if (not (equal? l 0))
                (begin
                  (add l D) ; add new decision to the decision sequence
                  (while-true))
                I))))) ; if I is empty, return I -- SAT
  (while-true))

; Unit resolution for CDCL -- takes in original knowledge base, gamma, and the decision sequence
(: cdcl-unit-res (-> (Listof (Listof Integer)) (Listof (Listof Integer)) (Listof Integer) (Listof Integer) (Values (Listof Integer) (Listof (Listof Integer)))))
(define (cdcl-unit-res kb G D I)
  (define comb-kb (append kb G (ints->clauses D))) ; combined knowledge base (U kb G D)
  (define unit-clause (find-unit-clause comb-kb)) ; find unit clause in combined knowledge base
  (if (empty? unit-clause) ; if a unit clause wasn't found
      (values (reverse I) kb) ; return current I and the current knowledge base
      (cdcl-unit-res ; otherwise, conidtion the combined knowledge base on the found unit clause and recurse
       (condition comb-kb (first unit-clause))
       empty ; no need to re-append in recursive call
       empty ; no need to re-append in recursive call
       (cons (first unit-clause) I))))

; Returns the asserting clause and assertion level
(: get-asserting (-> ImplGraph (Pairof (Listof Integer) Integer)))
(define (get-asserting graph)
  (let* ([keys (hash-keys (ImplGraph-nodes graph))]
         [hdl-node (get-hdl-node graph keys)]
         [all-paths (find-all-paths (ImplGraph-outgoing graph) hdl-node 0)]
         [first-uip (find-first-uip all-paths)]
         [asserting-clause (get-asserting-clause (ImplGraph-incoming graph) 0 first-uip)]
         [declevs (map (lambda ([cl : Integer]) (IG-Node-declev (hash-ref (ImplGraph-nodes graph) (- cl)))) asserting-clause)])
    (cons asserting-clause (get-assertion-level declevs))))

; Gets the asserting clause using a reverse BFS of the implication graph
(: get-asserting-clause (-> (HashTable Integer (Listof Integer)) Integer Integer (Listof Integer)))
(define (get-asserting-clause incoming-adj-list source target)
  (define (bfs [queue : (Listof Integer)] [visited : (Setof Integer)] [result : (Setof Integer)]) : (Listof Integer)
    (cond
      [(empty? queue) (set->list result)]
      [else
       (define current (first queue))
       (define rest-queue (rest queue))
       (define neighbors (hash-ref incoming-adj-list current))
       (cond
         ; Case where the current node is the first UIP -- skip and continue BFS
         [(equal? current target) (bfs rest-queue (set-add visited current) result)]
         ; Case where the current node is in the result conflict set
         [(set-member? result current)
          (cond
            [(empty? neighbors) ; decision node (no incoming)
             (bfs rest-queue (set-add visited current) result)] ; keep it in the conflict set (result)
            [else ; implication node (has incoming)
             (define new-queue 
               (append rest-queue
                       (for/list : (Listof Integer)
                         ([neighbor (in-list neighbors)]
                          #:when (not (set-member? visited neighbor)))
                         neighbor)))
             ; replace current with its neighbors in the conflict set (result)
             (define new-result 
               (set-union 
                (set-remove result current) 
                (list->set 
                 (for/list : (Listof Integer)
                   ([neighbor (in-list neighbors)]
                    #:when (not (set-member? visited neighbor)))
                   neighbor))))
             (bfs new-queue (set-add visited current) new-result)])] ; run again!
         ; Case where the current node isn't in the result conclict set
         [else 
          (define new-queue ; just add unvisited neighbors to the queue
            (append rest-queue
                    (for/list : (Listof Integer)
                      ([neighbor (in-list neighbors)]
                       #:when (not (set-member? visited neighbor)))
                      neighbor)))
          (bfs new-queue (set-add visited current) result)])]))
  (map - (bfs (list source) (set source) (set source)))) ; return negation of the result, giving us the asserting clause

; Finds the first UIP (the dominator node that is closest to the contradiction)
; Theoretically, if 'd' is a dominator node, it will be in the first path -- leveraging this
(: find-first-uip (-> (Listof (Listof Integer)) Integer))
(define (find-first-uip all-paths)
  (define (find-uips [first-path : (Listof Integer)]) : (Listof Integer)
    (match first-path
      ['(0) '()] ; don't include the contradiction, return
      [(cons f r)
       (if (andmap (lambda ([other-path : (Listof Integer)]) (contains other-path f)) (rest all-paths))
           (cons f (find-uips r))
           (find-uips r))]))
  (define uips (find-uips (first all-paths)))
  (last uips))

; Finds all possible paths from a source node to a target node in an ImplGraph -- Uses BFS
(: find-all-paths (-> (HashTable Integer (Listof Integer)) Integer Integer (Listof (Listof Integer))))
(define (find-all-paths outgoing-adj-list source target)
  (define (bfs [queue : (Listof (Listof Integer))] [result : (Listof (Listof Integer))]) : (Listof (Listof Integer))
    (cond
      [(empty? queue) result] ; return result if queue is empty
      [else
       (define path (first queue)) ; get the first path in the queue
       (define rest-queue (rest queue)) ; remove it (dequeue operation)
       (define node (last path)) ; get the last node in the path
       (define neighbors (hash-ref outgoing-adj-list node)) ; get the node's outgoing neighbors
       (cond
         [(equal? node target) ; if the last node is the target
          (bfs rest-queue (cons path result))] ; append the path to the result and run again!
         [else
          (define new-paths ; get the new possible paths
            (for/list : (Listof (Listof Integer)) ; for each neighbor, append it to the path (creates multiple paths)
              ([neighbor (in-list neighbors)])
              (append path (list neighbor))))
          (bfs (append rest-queue new-paths) result)])])) ; run again!
  (reverse (bfs (list (list source)) '()))) ; reverse to restore correct order

; Gets the decision node in an implication graph at the highest decision level
(define (get-hdl-node [graph : ImplGraph] [keys : (Listof Integer)]) : Integer
  (match keys
    ['() (error 'get-gdl-node "Couldn't find decision node made at the highest level.")]
    [(cons f r)
     (cond
       [(and (equal? (IG-Node-cause (hash-ref (ImplGraph-nodes graph) f)) #f) ; if decision node and declev equals declev of contradiction
             (equal? (IG-Node-declev (hash-ref (ImplGraph-nodes graph) f)) (IG-Node-declev (hash-ref (ImplGraph-nodes graph) 0))))
        f]
       [else (get-hdl-node graph r)])]))

; Gets the assertion level of an asserting clause
(define (get-assertion-level [levels : (Listof Integer)]) : Integer
  (if (equal? (length levels) 1)
      -1 ; case where the asserting clause is a unit clause
      (apply min levels))) ; return minimum decision level

; Builds an implication graph
; An implication graph is a data structure that shows us how we arrived at a contradiction from unit resolution
(: build-impl-graph (-> (Listof (Listof Integer)) (Listof Integer) (Listof Integer) ImplGraph))
(define (build-impl-graph kb D I)
  (define graph (make-impl-graph))
  (define temp D) ; copy of D -- after every added impl node, append the impl-lit to this
  (for ([i (in-range (length D))]) ; add decision nodes to the graph
    (add-dec! graph (list-ref D i) i 0))
  (let ([impls (set-diff I D)]) ; add implication nodes to the graph
    (for ([impl (in-list impls)])
      (find-impl-clause kb graph I temp impl 0)
      (set! temp (append temp (list impl))))) ; update temp
  (find-impl-clause kb graph I D 0 0) ; add the contradiction node to the graph
  graph)

; Finds the clause needed for a single implication to be made
(: find-impl-clause (-> (Listof (Listof Integer)) ImplGraph (Listof Integer) (Listof Integer) Integer Integer Void))
(define (find-impl-clause kb graph I D impl-lit kb-index) : Void
  (match kb
    ['() (error 'find-impl-clause "No implication clause found.\n")] ; error case
    [(cons f r)
     (cond
       ; Contradiction node case
       [(equal? impl-lit 0)
        (if (andmap (lambda ([x : Integer]) (contains I (- x))) f)
            (let ([nodes-from (map - (set-diff (first kb) (list impl-lit)))])
              (add-impl! ; add new implication node to the implication graph
               graph
               impl-lit ; vvv the declaration level will be the max decleration level of the preceding nodes
               (apply max (map IG-Node-declev (map (lambda ([key : Integer]) (hash-ref (ImplGraph-nodes graph) key)) nodes-from)))
               kb-index ; vvv the depth will be the min depth of the preceding nodes plus one
               (add1 (apply min (map IG-Node-depth (map (lambda ([key : Integer]) (hash-ref (ImplGraph-nodes graph) key)) nodes-from))))
               nodes-from))
            (find-impl-clause r graph I D impl-lit (add1 kb-index)))]
       ; Regular implication node case
       [(and (not (equal? (member impl-lit f) #f)) ; check that the implication literal is in the current clause
             (andmap (lambda ([x : Integer]) (contains (append (map - D) (list impl-lit)) x)) f)) ; check that each literal in a clause is also in I
        (let ([nodes-from (map - (set-diff (first kb) (list impl-lit)))])
         (add-impl! ; add new implication node to the implication graph
          graph
          impl-lit ; vvv the declaration level will be the max decleration level of the preceding nodes
          (apply max (map IG-Node-declev (map (lambda ([key : Integer]) (hash-ref (ImplGraph-nodes graph) key)) nodes-from)))
          kb-index ; vvv the depth will be the min depth of the preceding nodes plus one
          (add1 (apply min (map IG-Node-depth (map (lambda ([key : Integer]) (hash-ref (ImplGraph-nodes graph) key)) nodes-from))))
          nodes-from))]
       ; No match found, continue searching
       [else (find-impl-clause r graph I D impl-lit (add1 kb-index))])]))

; Selects a literal where neither l or ¬l are implied by unit resolution
; kb : knowledge base
; I : implications from unit resolution
(: next-decision (-> (Listof (Listof Integer)) (Listof Integer) Integer))
(define (next-decision kb I)
  (define implied-set (list->set I))
  (define candidates
    (for/fold ([vars : (Setof Integer) (set)])
              ([clause kb])
      (set-union vars (get-uniq-vars clause implied-set))))
  (if (set-empty? candidates)
      0
      (first (set->list candidates))))

; Helper for getting the next decision, returns a set of possible decision literals from a clause
(: get-uniq-vars (-> (Listof Integer) (Setof Integer) (Setof Integer)))
(define (get-uniq-vars clause implied-set)
  (for/fold ([result : (Setof Integer) (set)])
            ([lit clause])
    (if (and (not (set-member? implied-set lit))
             (not (set-member? implied-set (- lit))))
        (set-add result lit)
        result)))

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