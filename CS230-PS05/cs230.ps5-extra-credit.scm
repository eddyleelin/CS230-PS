(require racket/base)

(define turing-reduce
  (lambda (f a)
    (list (lambda () (f a) p))))

(define safe?
  (lambda (f a)
    (apply poly-safe? (turing-reduce f a))))

(define turing-reduce-2
  (lambda (f a)
    (list (eval (lambda () (f a) 0)))))

(define safe?-2
  (lambda (f a)
    (apply expr=zero? (turing-reduce-2 f a))))

(define factors
  (lambda (n)
    (letrec ((loop
             (lambda (try lst)
               (cond [(= try (- -1 n)) lst]
                     [(= try 0) (loop (- try 1) lst)]
                     [(zero? (remainder n try))
                      (loop (- try 1) (cons try lst))]
                     [else
                      (loop (- try 1) lst)]))))
      (loop n `()))))

(factors 10)
; ==> (1 2 5 10)
(factors 20)
; ==> (1 2 4 5 10 20)
(factors 12)
; ==> (1 2 3 4 6 12)
(factors 17)
; ==> (1 17)


; check-root takes in a list and checks if each one is a root
(define check-root
  (lambda (n coeffs try)
    (zero?
     (+ (expt try n)
        (apply
         +
         (foldr
          (lambda (elem init)
            (cons (* elem (expt try (length init))) init))
            `()
            coeffs))))))

(check-root 3 `(0 -7 6) -1)
; ==> #f
(check-root 3 `(0 -7 6) 0)
; ==> #f
(check-root 3 `(0 -7 6) 1)
; ==> #t
(check-root 3 `(0 -7 6) 2)
; ==> #t
(check-root 3 `(0 -7 6) (- 3))
; ==> #t

(define find-roots
  (lambda (n coeffs possible-roots)
    (foldr
     (lambda (root init)
       (if (check-root n coeffs root)
           (cons root init)
           init))
     `()
     possible-roots)))

(find-roots 3 `(0 -7 6) `(-3 -2 -1 0 1 2 3))
; ==> (-3 1 2)

(define p
  (lambda ((lst <list>))
    (let* [(n (car lst))
          (coeffs (last lst))
          (a-0 (last coeffs))
          (possible-roots (factors a-0))]
      (find-roots n coeffs possible-roots))))

(p (list 3 `(0 -7 6)))
; ==> (-3 1 2)
      
          