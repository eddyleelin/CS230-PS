(require racket/base)
(require racket/stream)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part One
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

(define Cart
  (lambda ((a <list>) (b <list>))
    (foldr (lambda (a_el lst) ; for every element in a, ...
             ; append itself to every element of b
             (append 
              (map (lambda (b_el)
                            (list a_el b_el))
                          b)
                     lst)) ; 
           `()
           a)))


(define alist `(1 2 3 4 5))
(define blist `(6 7 8))

(display "Cartesian:\n")
(Cart alist blist)
; => ((1 6) (1 7) (1 8) (2 6) (2 7) (2 8) (3 6) (3 7) (3 8) (4 6) (4 7) (4 8) (5 6) (5 7) (5 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

(define fcn?
  (lambda (S) ; S is a set of ordered pairs, assuming no duplicated ordered pairs
    ; for each ordered pair in S,
    ; >> compare to the rest of S, to see if the same first entry exists in other pairs
    ; base case: S is empty, return #t
    (cond [(null? S) #t]
          ; otherwise, compare (car (car S)), first entry) with
          ;    (map car (cdr S)) to see if it is a member
          ;     >> if it is a member, return #f 
          [(member (car (car S))
                    (map car (cdr S))) #f]
          ; else, check the rest of the list
          [else
           (fcn? (cdr S))])))

(display "fcn?:\n")
(fcn? `((1 2) (2 4) (3 6) (4 8)))
; ==> #t
(fcn? `((1 2) (2 4) (3 6) (4 6)))
; ==> #t
(fcn? (Cart alist blist))
; ==> #f
(fcn? (Cart `(1 2 2) blist))
; ==> #f
(fcn? `((1 2) (2 4) (3 6) (3 8)))
; ==> #f


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3
(define (remove-duplicates l)
  (cond [(null? l)
         '()]
        [(member (car l) (cdr l))
         (remove-duplicates (cdr l))]
        [else
         (cons (car l) (remove-duplicates (cdr l)))]))

(define pi1
  (lambda (S)
    (remove-duplicates (map car S))))

(define pi2
  (lambda (S)
    (remove-duplicates (map cadr S))))

(display "pi1:\n")
(pi1 `((1 2) (2 4) (3 6) (4 8)))
; ==> (1 2 3 4)
(pi1 `((1 2) (2 4) (3 6) (4 6)))
; ==> (1 2 3 4)
(pi1 (Cart alist blist))
; ==> (1 2 3 4 5)
(pi1 (Cart `(1 2 2) blist))
; ==> (1 2)

(display "pi2:\n")
(pi2 `((1 2) (2 4) (3 6) (4 8)))
; ==> (2 4 6 8)
(pi2 `((1 2) (2 4) (3 6) (4 6)))
; ==> (2 4 6)
(pi2 (Cart alist blist))
; ==> (6 7 8)
(pi2 (Cart `(1 2 2) blist))
; ==> (6 7 8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4

(define diag
  (lambda (A)
    (foldr (lambda (el lst)
             (cons (list el el) lst))
           `()
           A)))
             
(display "diag:\n")
(diag alist)
; ==> ((1 1) (2 2) (3 3) (4 4) (5 5))
(diag blist)
; ==> ((6 6) (7 7) (8 8))
(diag `(10 20 30 40 50))
; ==> ((10 10) (20 20) (30 30) (40 40) (50 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5

(define diag?
  (lambda (D)
    (cond
      [(null? D) #t]
      [(not (= (caar D) (cadar D)))
       #f]
      [else
       (diag? (cdr D))])))

(display "diag?:\n")
(display "  #t tests:\n")
(diag? (diag alist))
; ==> #t
(diag? (diag blist))
; ==> #t
(diag? (diag `(10 20 30 40 50)))
; ==> #t
(diag? `((1 1) (2 2) (3 3) (4 4)))
; ==> #t
(display "  #f tests:\n")
(diag? `((0 1)))
; ==> #f
(diag? `((1 2)))
; ==> #f
(diag? `((1 1) (2 2) (3 3) (4 4) (5 6)))
; ==> #f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6

(define diag-inv
  (lambda (Delta)
    (foldr
     (lambda (elem lst)
       (cons (car elem) lst))
     `()
     Delta)))

(display "diag-inv:\n")
(diag-inv (diag alist))
; ==> (1 2 3 4 5)
(diag-inv (diag blist))
; ==> (6 7 8)
(diag-inv (diag `(10 20 30 40 50)))
; ==> (10 20 30 40 50)
(diag-inv `((1 1) (2 2) (3 3) (4 4)))
; ==> (1 2 3 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7

(define powerset
  (lambda (A)
    (foldr (lambda (elem lst)
             (append
              lst
              (map (lambda (lst1)
                     (cons elem lst1))
                   lst)))
           `(())
           A)))

(display "powerset:\n")
(powerset blist)
; ==> (() (8) (7) (7 8) (6) (6 8) (6 7) (6 7 8))
(powerset `(1))
; ==> (() (1))
(powerset `(1 2))
; ==> (() (2) (1) (1 2))
(powerset `(1 2 3))
; ==> (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(powerset `(1 2 3 4))
; ==> (() (4) (3) (3 4) (2) (2 4) (2 3) (2 3 4) (1) (1 4) (1 3) (1 3 4) (1 2) (1 2 4) (1 2 3) (1 2 3 4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part Two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stream Definitions to Help
(define ones (stream-cons 1 ones))
(define add-streams 
  (lambda ((a <stream>) (b <stream>))
    (cond ((stream-empty? a) b)
          ((stream-empty? b) a)
          (else (stream-cons (+ (stream-first a) (stream-first b))
                                (add-streams (stream-rest a) (stream-rest b)))))))

(define scale-stream
  (lambda ((s <stream>) (factor <number>))
    (stream-map (lambda (x) (* x factor)) s)))

(define print-crlf 
  (lambda ()
    (printf "
")))

(define print-stream 
  (lambda ((s <stream>) (n <integer>))
    (cond ((zero? n) (printf "..."))
          (else  (print (stream-first s))
                 (printf " ")
                 (print-stream (stream-rest s) (- n 1))))))

(define stream-null?
  (let ((end-of-stream?
          (lambda (x)
            (eq? x "end of stream"))))
    (compose end-of-stream? stream-first)))

(define stream-cdr
  stream-rest)


;(print-stream ones 10)
;(print-stream (add-streams ones ones) 10)
(define integers (stream-cons 1 (add-streams ones integers)))
;(print-stream integers 100)

;; this is just like print-stream which is how Bruce Donald wrote it
(define stream->listn
  (lambda ((s <stream>) (n <integer>))
    (cond ((or (zero? n) (stream-empty? s)) '())
          (else (cons (stream-first s)
                      (stream->listn (stream-rest s) (- n 1)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem II-1

(define flatten
  (lambda (A)
    ; if the first element of the stream A is a list with only one value,
    (cond [(null? (cdr (stream-first A)))
           ; cons the list element...
           (stream-cons (car (stream-first A))
                         ; to the rest of the stream A
                        (flatten (stream-rest A)))]
          ; otherwise, if (stream-first A) is a list of more than one ordered pair...
          [else
           ; cons the list element...
           (stream-cons (car (stream-first A))
                        ; to the flattened stream with...
                        (flatten
                         ; the first pair of the first list removed
                         (stream-cons (cdr (stream-first A))
                                      ; and the rest of the stream
                                      (stream-rest A))))]))) 

(define stream-cart
  (lambda (A B)
    ; count up i as all integers starting from 1.
    (flatten (for/stream ([i integers])
                ; count up k starting from 1 to i.
                 (for/list ([k (stream->listn integers i)])
                   ; get A[k-1], starting from 0 and going up
                   (list (stream-ref A (- k 1))
                         ; get B[i-k], starting from i and going down
                         (stream-ref B (- i k)))))))) 

(display "stream-cart:\n")
(define triangular
  (stream-cons 3 (add-streams (add-streams (scale-stream ones 2) integers) triangular)))
(define fibs 
  (stream-cons 0
               (stream-cons 1
                            (add-streams fibs (stream-rest fibs)))))

(stream->listn (stream-cart integers integers) 20)
; ==> ((1 1) (1 2) (2 1) (1 3) (2 2) (3 1) (1 4) (2 3) (3 2) (4 1) (1 5) (2 4) (3 3) (4 2) (5 1) (1 6) (2 5) (3 4) (4 3) (5 2))
(stream->listn (stream-cart ones integers) 20) ; note: ones is not a set, but it is a stream
; ==> ((1 1) (1 2) (1 1) (1 3) (1 2) (1 1) (1 4) (1 3) (1 2) (1 1) (1 5) (1 4) (1 3) (1 2) (1 1) (1 6) (1 5) (1 4) (1 3) (1 2))
(stream->listn (stream-cart integers fibs) 20) ; note: fibs is not a set, but it is a stream
; ==> ((1 0) (1 1) (2 0) (1 1) (2 1) (3 0) (1 2) (2 1) (3 1) (4 0) (1 3) (2 2) (3 1) (4 1) (5 0) (1 5) (2 3) (3 2) (4 1) (5 1))
(stream->listn (stream-cart triangular fibs) 20)
; ==> ((1 0) (1 1) (1 0) (1 1) (1 1) (1 0) (1 2) (1 1) (1 1) (1 0) (1 3) (1 2) (1 1) (1 1) (1 0) (1 5) (1 3) (1 2) (1 1) (1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem II-3

(define stream-pi1
  (lambda (S)
    (stream-cons (car (stream-first S))
                 (stream-pi1 (stream-rest S)))))

(define stream-pi2
  (lambda (S)
    (stream-cons (cadr (stream-first S))
                 (stream-pi2 (stream-rest S)))))

(display "stream-pi1:\n")
(stream->listn (stream-pi1 (stream-cart integers integers)) 10)
; ==> (1 2 3 4)
(stream->listn (stream-pi1 (stream-cart integers (scale-stream integers 2))) 10)
; ==> (1 2 3 4)
(stream-pi1 (Cart ones blist))
; ==> (1 2 3 4 5)
(stream-pi1 (Cart `(1 2 2) blist))
; ==> (1 2)

(display "stream-pi2:\n")
(stream-pi2 `((1 2) (2 4) (3 6) (4 8)))
; ==> (2 4 6 8)
(stream-pi2 `((1 2) (2 4) (3 6) (4 6)))
; ==> (2 4 6)
(stream-pi2 (Cart alist blist))
; ==> (6 7 8)
(stream-pi2 (Cart `(1 2 2) blist))
; ==> (6 7 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
