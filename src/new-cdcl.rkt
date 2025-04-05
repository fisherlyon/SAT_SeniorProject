#lang typed/racket

(require "sat-util.rkt")
(require "read-cnf.rkt")

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
  (define (while-true) : (U Boolean (Listof (Listof Integer)))
    (let-values ([(I new-kb) (cdcl-unit-res kb G (unbox D) I)])
      (if (equal? new-kb '{{}}) ; if unit resolution detects a contradiction
          (if (empty? (unbox D)) ; contradiction without any decisions
              #f ; so return false -- UNSAT
              (let* ([graph (build-impl-graph (append kb G) (unbox D) I)] ; build the implication graph
                     [first-uip (get-first-uip graph)] ; find the first UIP
                     [asserting (get-asserting-clause graph first-uip)] ;  get the asserting info
                     [a (car asserting)] ; get asserting clause
                     [m (cdr asserting)]) ; get assertion level
                (erase m D) ; erase decisons past the resulting assertion level
                (set! G (append G (list a))) ; add the learned/asserting clause to G
                (while-true)))
          #t)) ;; TO-DO
    #f)
  #f)

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

; Finds the shortest path in ImplGraph given source and target nodes -- Uses BFS
(: find-shortest-path (-> (HashTable Integer (Listof Integer)) Integer Integer (U Boolean (Listof Integer))))
(define (find-shortest-path outgoing-adj-edges source target)
  (define (bfs [queue : (Listof (Listof Integer))]
               [visited : (Listof Integer)])
    : (U Boolean (Listof Integer))
    (if (empty? queue)
        #f
        (let* ([path (first queue)]
               [dq (rest queue)]
               [node (last path)]
               [neighbors : (Listof Integer) (hash-ref outgoing-adj-edges node)])
          (if (equal? node target)
              path
              (let-values ([(new-paths new-visited)
                            (for/fold ([paths : (Listof (Listof Integer)) '()]
                                       [vis : (Listof Integer) visited])
                                      ([neighbor : Integer (in-list neighbors)]
                                       #:when (not (member neighbor visited)))
                              (values (cons (append path (list neighbor)) paths)
                                      (cons neighbor vis)))])
                (bfs (append dq (reverse new-paths)) new-visited))))))
  (bfs (list (list source)) (list source)))

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

; Returns the asserting clause and assertion level given the first uip
(: get-asserting-clause (-> ImplGraph Integer (Pairof (Listof Integer) Integer)))
(define (get-asserting-clause graph first-uip)
  (let* ([asserting-clause (map - (hash-ref (ImplGraph-incoming graph) first-uip))] ; incoming edges for the first UIP
         [declevs (map (lambda ([cl : Integer]) (IG-Node-declev (hash-ref (ImplGraph-nodes graph) (- cl)))) asserting-clause)]) ; decision levels of incoming edges
    (cons asserting-clause (get-assertion-level declevs))))

; Gets the asserting clause that is the first UIP (unique implication point)
; Returns the clause and its assertion level
; A UIP is a variable setting at the highest level that lies on every
; path from the latest decision to the contradiction.
; The first UIP is the UIP that is closest to the contradiction.
(: get-first-uip (-> ImplGraph Integer))
(define (get-first-uip graph)
  (let* ([keys (hash-keys (ImplGraph-nodes graph))]
         [hdl-node (get-hdl-node graph keys)] ; node at the highest decision level
         [shortest-path (find-shortest-path (ImplGraph-outgoing graph) hdl-node 0)]) ; shortest path from hdl-node to contradiciton
    (if (not (boolean? shortest-path))
        (last (drop-right shortest-path 1)) ; the first UIP is the last implication before the contadiction
        (error 'get-first-uip "Shortest-path returned false -- first uip not found."))))

; Builds an implication graph
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


(define (test) : ImplGraph
  (define graph : ImplGraph (make-impl-graph))
  (add-dec! graph 1 0 0)
  (add-dec! graph 2 1 0)
  (add-dec! graph 4 2 0)
  (add-impl! graph 5 2 4 1 '(1 4))
  graph)