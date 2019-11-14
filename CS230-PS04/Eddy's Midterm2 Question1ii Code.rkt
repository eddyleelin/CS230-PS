(require racket/base)
(require racket/stream)

(define I-am-very-elegant
  (lambda (lst)
    (reverse (foldl (lambda (e a)
                      (if (not (member e a))
                          (cons e a)
                          a))
                    `()
                    lst))))
                    

(define powerlist
  (lambda ((l <list>))
    (foldr (lambda (e lst)
             (append
              (map (lambda (lst1)
                     (cons e lst1))
                   lst)
              lst))
           `(())
           (I-am-very-elegant l))))

(define lst `(1 2 3 4 4 4))

(powerlist lst)