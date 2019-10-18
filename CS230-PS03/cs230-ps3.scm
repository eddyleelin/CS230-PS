;;;
;;;cs230.ps3.scm
;;;
;;; For this problem set, when defining functions do not use types in 
;;; the lambda expressions. Instead, you should add a comment as to what 
;;; the type should be.

;;; Do not use (require racket/base) for this problem set.
;; ----- Useful functions -----

;; Define a predicate member? that returns #t if obj is a member of
;; lst and #f otherwise.

;; Contrast with the builtin member function, which returns the
;; sublist of lst starting with obj if obj is in the list.
(require racket/class)

(define member?
  (lambda (obj lst)
    (not (not (member obj lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;foldr and foldl are provided in scheme                                                                                                                                           
;;                                                                                                                                                                                 
;;(define accumulate                                                                                                                                                               
;; (lambda (initial op l)                                                                                                                                                          
;;    (cond ((null? l) initial)                                                                                                                                                    
;;      (else                                                                                                                                                                      
;;        (op (car l) (accumulate initial op (cdr l)))))))                                                                                                                         
;;                                                                                                                                                                                 
;;Note: (accumulate  '() cons '(1 2 3 4)) => '(1 2 3 4)
;;(define foldr (lambda (op init lst) (accumulate init op lst)))                                         
;;Note: (foldr cons '() '(1 2 3 4)) => '(1 2 3 4)                                                                              
;;Whereas: (foldl cons '() '(1 2 3 4) => '(4 3 2 1)  
;; ------ Data type definitions -----

;; Directed graph class definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(vertices <graph>) => list of vertices
;;(edges <graph>) => list of edges 
;;
(defclass <graph> ()
  (vertices :initarg :vertices :accessor vertices) 
  (edges :initarg :edges :accessor edges))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(name <vertex>) => the name of the vertex
;;ex: (name (make-vertex 'a)) => a
;;
(defclass <vertex> ()
  (name :initarg :name :accessor name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(start <directed-edge>) => first vertex of directed-edge
;;ex: (start (v1 v2)) => v1
;;(finish <directed-edge>) => second vertex of directed-edge
;;ex: (finish (v1 v2)) => v2
;;
(defclass <directed-edge> ()
  (start :initarg :start :accessor start)
  (finish :initarg :finish :accessor finish))

(define make-vertex
  (lambda (name)
    (make <vertex> :name name)))

(define make-edge
  (lambda (a b) ;a <vertex> b <vertex>
    (make <directed-edge> :start a :finish b)))

;; Two vertices are considered equal if their names are equal

(define equal-vertex?
  (lambda (v1 v2)
    (eq? (name v1) (name v2))))

;; lookup-vertex takes a name and a list of vertices, and finds a vertex
;; with that name.  Useful when you have the name of a vertex and need
;; the vertex itself.

(define lookup-vertex
  (lambda (vname vlist)
    (cond ((null? vlist) #f)
          ((equal? vname (name (first vlist))) (first vlist)) ;replaced car with first
          (else (lookup-vertex vname (rest vlist)))))) ;replaced cdr with rest

;; make-graph takes two lists whose atoms are symbols, one of the form
;;   (v1 v2 v3 ...) 
;; which becomes the list of vertices and the other of the form
;;   ((u1 u2) (u3 u4) ...) 
;; which becomes the list of edges.

(define make-graph
  (lambda (v-names e-list)
    (let* ((v (map make-vertex v-names))
           (create-edge 
              (lambda (name1 name2)
                (make-edge (lookup-vertex name1 v)
                           (lookup-vertex name2 v)))))
        (make <graph>
              :vertices v        
              :edges (map create-edge
                          (map first e-list)
                          (map second e-list))))))

;; Convert a list of vertices to a list of names of vertices

(define name-vertices
  (lambda (vlist)
    (map name vlist)))

;;;Same as standard member function but works with vertices
(define member-vertices 
  (lambda (a lat) 
    (cond ((null? lat) #f) 
          ((equal-vertex? a (car lat)) lat) 
          (else (member-vertices a (cdr lat))))))

;; Find the set difference of two sets represented as lists.  That is,
;; return a list consisting of everything in list1 that is not in
;; list2

(define set-diff-vertices
  (lambda (list1 list2)
    (cond ((null? list1) '()) 
          ((member-vertices (car list1) list2) (set-diff-vertices (cdr list1) list2))
          (#t (cons (car list1) (set-diff-vertices (cdr list1) list2))))))

;; Take the union of two sets represented as lists -- no duplicates

(define union
  (lambda (list1 list2)
    (cond ((null? list1) list2) 
          ((member (car list1) list2) (union (cdr list1) list2))
          (else (cons (car list1) (union (cdr list1) list2))))))
          

;; Take the intersection of two sets represented as lists 

(define intersection
  (lambda (list1 list2)
    (cond ((null? list1) '()) 
          ((member (car list1) list2) 
             (cons (car list1) (intersection (cdr list1) list2)))
          (else (intersection (cdr list1) list2)))))

;;; ----- TESTING EXAMPLES -----

;; ----- Problem 1 -----

(define exits
  (lambda (v G)
    (foldr (lambda (new l)
             (cons (finish new) l))
           `()
           (filter
            (lambda (edge)
              (equal-vertex? (start edge) v))
            (edges G)))))
                                 
    

(define g1 (make-graph '(a b c d e) 
		       '((a b) (a c) (b c) (b e) (c d) (d b))))

(name-vertices (exits (lookup-vertex 'b (vertices g1)) g1))

(name-vertices (exits (lookup-vertex 'e (vertices g1)) g1))


(define verify-path
  (lambda (g lst)
    (cond ((null? lst) #t) ;if no more in lst, it works
          ((not (member-vertices (car lst) (vertices g))) #f)
          ((null? (cdr lst))
           #t)
          ((not (member-vertices (car (cdr lst))
                                 (exits (car lst) g)))
           #f)
          (else
           (verify-path g (cdr lst))))))

(verify-path g1 
   (map (lambda (x) (lookup-vertex x (vertices g1))) '(a b c d b e)))
; ==> #t
  
(verify-path g1 
   (map (lambda (x) (lookup-vertex x (vertices g1))) '(a b c d e)))
; ==> #f
  
;; ----- Problem 2 -----
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(label <labeled-edge>) => label of labeled-edge
;;ex: (label (make-labeled-edge a b l)) => l
;;
(defclass <labeled-edge> (<directed-edge>)
  (label :initarg :label :accessor label))

(define make-labeled-edge
  (lambda (a b l) ;a <vertex> b <vertex> l <obj>
    (make <labeled-edge> :start a :finish b :label l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(start-state <automaton>) => start-state of automaton
;;(final-states <automaton>) => list of final-states of automaton
;;    
(defclass <automaton> (<graph>)
  (start-state :initarg :start-state :accessor start-state)
  (final-states :initarg :final-states :accessor final-states)) ;start-state <symbol> final-states <list>
;;note: all accessors that apply to graph apply to automaton
  
;; make-automaton takes four parameters.  
;; The first is a list of symbols of the form (v1 v2 v3 ...) which 
;;   becomes the list of vertices.
;; The second is a list of triples of the form 
;;   ((u1 u2 l1) (u3 u4 l2) ...) which becomes the list of labeled
;;   edges (with the u's symbols which represent vertices and the l's 
;;   objects which become the labels).
;; The third is a single symbol for the start state.
;; The fourth is a list of symbols that represent final states.

(define make-automaton
  (lambda (v-names e-list s-state f-states) ;v-names <list>, e-list <list>, s-state <symbol>, f-states <list>
    (let* ((v (map make-vertex v-names))
           (create-labeled-edge 
              (lambda (name1 name2 label)
                (make-labeled-edge (lookup-vertex name1 v) 
                                   (lookup-vertex name2 v) 
                                   label))))
      (make <automaton>
            :vertices v
            :edges (map create-labeled-edge
                        (map first e-list)
                        (map second e-list)
                        (map third e-list))
            :start-state s-state
            :final-states f-states))))

(define dfa1
  (make-automaton '(a b c) 
	    '((a a 0) (a b 1) (b a 1) (b c 0) (c b 0) (c c 1))
            'a '(a)))

;;; ----- Problem 3 -----
(define step-dfa
  (lambda (dfa state input)
    (let ((dfa-exits (filter (lambda (edge) (= (label edge) input))
                             (filter
                              (lambda (edge)
                                (eq? (name (start edge)) state))
                              (edges dfa)))))
      (if (not (= (length dfa-exits) 1)) #f
          (name (finish (car dfa-exits)))
          ))))

(step-dfa dfa1 'c 1) ; ==> c
(step-dfa dfa1 'd 0) ;==> #f
(step-dfa dfa1 'a 0) ;==> a
(step-dfa dfa1 'a 1) ;==> b
(step-dfa dfa1 'a 2) ;==> #f

(define bad-dfa
  (make-automaton '(a b c) 
 	    '((a a 0) (a b 0) (b a 1) (b c 0) (c b 0) (c c 1))
            'a '(a)))

(step-dfa bad-dfa 'a 0); ==> #f

;; ----- Problem 4 -----

(define simulate-dfa
  (lambda (dfa lst)
    (letrec ((loop
             (lambda (state inputs)
               (cond ((null? inputs)
                      (member? state (final-states dfa)))
                     (else
                      (loop (step-dfa dfa state (car inputs)) (cdr inputs)))))))
      (loop (start-state dfa) lst))))
                          
 `"Simulate-dfa:"
 (simulate-dfa dfa1 '(1 0 0 1)) ;==> #t
 (simulate-dfa dfa1 '(1 0 1 1)) ;==> #f

(define integer->binary
  (lambda (n)
    (cond ((eq? n 0) '())
	  (else (append (integer->binary (quotient n 2)) 
                        (list (if (even? n) 0 1)))))))
          
 (simulate-dfa dfa1 (integer->binary 12)) ;==>#t
 (simulate-dfa dfa1 (integer->binary 10)) ;==>#f

;; ----- Problem 5 -----
(define step-nfa
  (lambda (dfa states input)
    (let ((finishes (map finish (filter (lambda (edge)
             (= (label edge) input))
           (filter (lambda (edge)
            (member? (name (start edge)) states))
          (edges dfa))))))
      (if (= (length finishes) 0) #f (map name finishes)))))


; base case: inputs is null, have a list of states.
;                       -> Compare states to see if any are final states
;                             -> Take intersection of states and final states, check length
; if inputs not null: rerun loop with new states being this step's final states

(define convert-to-list
  (lambda (var)
    (if (list? var)
        var
        (list var))))

(define simulate-nfa
  (lambda (dfa lst)
    (letrec ((loop
             (lambda (state-given inputs)
               (let ((states (convert-to-list state-given)))
               (cond ((null? inputs)
                      (not (= (length
                               (intersection states
                                             (final-states dfa)))
                              0)))
                      ((not (list? states))
                       (loop (step-nfa dfa `(states) (car inputs)) (cdr inputs)))
                      (else
                       (loop (step-nfa dfa states (car inputs)) (cdr inputs))))))))
      (loop (start-state dfa) lst))))

(define nfa1
  (make-automaton '(a b c d e)
	    '((a a 0) (a a 1) (a b 1) (a c 0) (b d 1) (c e 0)
	      (d d 0) (d d 1) (e e 0) (e e 1))
	    'a
	    '(d e)))

"step-nfa:"
(step-nfa nfa1 `(a b) 0) ;==>(a c)
(step-nfa nfa1 `(f) 0) ;==> #f
(step-nfa nfa1 '(b) 1) ;==>(d)
(step-nfa nfa1 '(e e) 1) ;==> (e)

"simulate-nfa:"
(simulate-nfa nfa1 `()) ;==>#f
(simulate-nfa nfa1 `(0)) ;==>#f
(simulate-nfa nfa1 `(1)) ;==>#f
(simulate-nfa nfa1 `(0 1)) ;==>#f
(simulate-nfa nfa1 `(1 1 1)) ;==>#t
(simulate-nfa nfa1 `(1 1 1 1)) ;==>#t
(simulate-nfa nfa1 `(1 1 1 0 0 0)) ;==>#t
            
;; ----- Problem 6 -----

; step-graph returns a list of the vertex objects that can be accessed from the
; current vertices passed to step-graph (including themselves)
; param G: graph
; param states: states (list of vertex objects)
; param visited: visited vertices (list of vertex objects)
; param des: destination vertex (vertex object)
(define path-exits
  (lambda (G states visited des)
    ; -> check to see if any of the states are the destination, if so, return states
    (cond [(member-vertices des states) states]
          ; -> if not
          [else
           (let* ; add states to visited
              ([new-visited (union visited states)]
              ; find all possible exits...
               [possible-exits (map finish (filter (lambda (edge) 
                                          (member-vertices (start edge) states))
                                        (edges G)))]
              ; -> find set-diff-vertices between possible exits and visited
              [unvisited (set-diff-vertices possible-exits new-visited)])
              ; -> if the set-diff-vertices is empty, return states
            (cond [(null? unvisited) states]
                  [else
                   ; -> else, add each one not in visited to states, rerun the computation
                   (path-exits G (union new-visited unvisited) new-visited des)]))])))

"path-exits:"
(path-exits g1 (list (lookup-vertex 'a (vertices g1))) `() (lookup-vertex 'a (vertices g1)))
(path-exits g1 (list (lookup-vertex 'a (vertices g1))) `() (lookup-vertex 'b (vertices g1)))
(path-exits g1 (list (lookup-vertex 'a (vertices g1))) `() (lookup-vertex 'e (vertices g1)))
(path-exits g1 (list (lookup-vertex 'e (vertices g1))) `() (lookup-vertex 'e (vertices g1)))
(path-exits g1 (list (lookup-vertex 'e (vertices g1))) `() (lookup-vertex 'b (vertices g1)))


(define path?
  (lambda (ori-name des-name G)
    (let* ([ori (lookup-vertex ori-name (vertices G))]
          [des (lookup-vertex des-name (vertices G))]
          [ori-list (list ori)]
          [all-exits (path-exits G ori-list `() des)])
      (cond [(boolean? (member-vertices des all-exits))
             #f]
            [else
             #t]))))

(define g2 (make-graph '(a b c) '((a b) (b a) (a c) (c a) (b c))))
(define g3 (make-graph '(a b c d) '((a b) (b c) (a c) (c b) (d b))))
(define g4 (make-graph '(a b c d) '((a b) (a c) (b a) (c a) (a d) (b c) (c b))))

"path?:"
 (path? 'a 'e g1) ;==> #t
 (path? 'd 'a g1) ;==> #f
 (path? 'a 'c g2) ;==> #t
 (path? 'c 'b g2) ;==> #t
 (path? 'd 'd g3) ;==> #t
 (path? 'a 'd g3) ;==> #f
 (path? 'b 'd g4) ;==> #t


;; ----- Problem 7 -----
(defclass <vertex+parent> (<vertex>)
  (parent :initarg :parent :accessor parent))

(define make-vertex+parent
  (lambda (v p) ;v <vertex>, p <obj>                                                                                                                                               
    (make <vertex+parent> :name (name v) :parent p)))

; find-path returns the path (as a list) from the origin to the destination,
; if there is one. Otherwise, it returns #f.
(define find-path
  (lambda (ori-name des-name G)
    (let* ([ori (lookup-vertex ori-name (vertices G))]
           [des (lookup-vertex des-name (vertices G))])
    (find-path-helper ori des G `()))))


; find-path-helper performs the bulk of find-path, with a visited parameter
; param visited: a list of vertex objects that have been visited
(define (find-path-helper ori des G visited)
  (let* ; ori, des be the names of the vertex objects
      ([new-visited (union visited (list ori))])
    (cond
      ; if the origin is the destination, return it
      [(equal-vertex? ori des) (list des)]
      ; otherwise, check if there's a valid path on each exit of ori
      [else
       (let ([new-vertices (set-diff-vertices (exits ori G) new-visited)])
         (cond
           [(null? new-vertices) #f]
           [(not new-vertices) #f]
           [(not (car new-vertices)) #f]
           [else
            (let ([possible-path
                   (path-list (set-diff-vertices (exits ori G) new-visited) des G new-visited)])
              (cond ; then,
                ; check if possible-path is #f (a boolean), if so, return #f
                [(boolean? possible-path) #f]
                [else (cons ori possible-path)]))]))])))

; given multiple starting states, determine if there is a path.
; Return such a path if it exists. If not, return #f
(define path-list
  (lambda (oris D G visited)
    (let ([new-visited (union visited oris)])
    (cond
      [(empty? oris) #f]
      [else
       (let ([possible-route
              (find-path-helper (car oris) D G new-visited)])
         (cond
           [(boolean? possible-route) (path-list (cdr oris) D G new-visited)]
           [else possible-route]))]))))



"find-path:"
(name-vertices (find-path 'a 'e g1)) ;==> (a b e)
(find-path 'd 'a  g1)                   ;==> #f
(name-vertices (find-path 'a 'c g2)) ;==> (a c) or (a b c)
(name-vertices (find-path 'c 'b g2)) ;==> (c a b)
(name-vertices (find-path 'd 'd g3)) ;==> (d)
(find-path 'a 'd g3)                 ;   ==> #f
(name-vertices (find-path 'b 'd g4)) ;==> (b a d)

