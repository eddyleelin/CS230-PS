(require racket/base)
(require racket/stream)
;;; See http://docs.racket-lang.org/reference/streams.html?q=stream#%28tech._stream%29
; 
; A stream is a kind of sequence that supports functional iteration via stream-first and stream-rest.
; The stream-cons special form constructs a lazy stream.

  
(define ones  (stream-cons 1 ones))

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

;(print-stream ones 10)
;(print-stream (add-streams ones ones) 10)
(define integers (stream-cons 1 (add-streams ones integers)))
;(print-stream integers 100)

;; this is just like print-stream which is how I wrote it
(define stream->listn
  (lambda ((s <stream>) (n <integer>))
    (cond ((or (zero? n) (stream-empty? s)) '())
          (else (cons (stream-first s)
                      (stream->listn (stream-rest s) (- n 1)))))))

(stream->listn integers 20)

(define mul-streams 
  (lambda ((a <stream>) (b <stream>))
    (cond ((stream-empty? a) b)
          ((stream-empty? b) a)
          (else (stream-cons (* (stream-first a) (stream-first b))
                                (mul-streams (stream-rest a) (stream-rest b)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print-crlf)
(print-crlf)
(define squares (mul-streams integers integers))
(print-stream squares 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Factorial
(define factorial 
  (lambda (n)
    (if (< n 2) 1
        (* n (factorial (- n 1))))))

(factorial 20)

(print-crlf)
(define fact (mul-streams integers  (stream-cons 1 fact)))
;(print-stream fact 300)
(print-stream fact 30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fibonacci numbers

(define fibonacci
  (lambda ((n <integer>))
    (if (< n 2)
        n
        (+ (fibonacci (- n 1))
           (fibonacci (- n 2))))))
(fibonacci 6)

(define fibs 
  (stream-cons 0
               (stream-cons 1
                            (add-streams fibs (stream-rest fibs)))))

(stream->listn fibs 100)

(print-crlf)
(fibonacci 15)
(last (stream->listn fibs 16))
