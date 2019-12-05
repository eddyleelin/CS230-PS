(require racket/base)
(require racket/stream)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part Two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

(define viterbiProb
  (lambda (emission k switchPr p1 p2)
    (letrec
        ([pathSolve
          (lambda (k i v1 v2)
            (cond [(> i k) v1]
                  [else
    (let*
      ([heads? (= 0 (list-ref emission (- i 1)))]
       [py1 (if heads? p1 (- 1 p1))]
       [py2 (if heads? p2 (- 1 p2))]
       [v11 (* v1 (- 1 switchPr) py1)]
       [v12 (* v2 switchPr py1)]
       [v21 (* v1 switchPr py2)]
       [v22 (* v2 (- 1 switchPr) py2)]
       [next-v1 (if (> v11 v12) v11 v12)]
       [next-v2 (if (> v21 v22) v21 v22)])
            (cond
              [(= i 1)
               (pathSolve k (+ i 1) (* 0.5 py1) (* 0.5 py2))]
              [else
               (pathSolve k (+ i 1) next-v1 next-v2)]))]))])
      (pathSolve k 1 0 0))))

(viterbiProb '(0 0 1 1 0 1) 5 0.45 0.5 0.65)
;=> 0.0019770029296875004
(viterbiProb '(1 0 1 0) 2 0.45 0.5 0.65)
;=> 0.06875

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5

(define viterbiPath
  (lambda (emission switchPr s1 s2 p1 p2)
    (letrec
        ([pathSolve
          (lambda (k i v1 path1 v2 path2)
            (cond [(> i k)
               (if (> v1 v2)
                   (list path1 v1)
                   (list path2 v2))]
                  [else
    (let*
      ([heads? (= 0 (list-ref emission (- i 1)))]
       [py1 (if heads? p1 (- 1 p1))]
       [py2 (if heads? p2 (- 1 p2))]
       [v11 (* v1 (- 1 switchPr) py1)]
       [v12 (* v2 switchPr py1)]
       [v21 (* v1 switchPr py2)]
       [v22 (* v2 (- 1 switchPr) py2)]
       [next-v1 (if (> v11 v12) v11 v12)]
       [next-path1 (if (> v11 v12)
                       (append path1 (list s1))
                       (append path2 (list s1)))]
       [next-v2 (if (> v21 v22) v21 v22)]
       [next-path2 (if (> v21 v22)
                       (append path1 (list s2))
                       (append path2 (list s2)))])
            (cond
              [(= i 1)
               (pathSolve k (+ i 1) (* 0.5 py1) (list s1) (* 0.5 py2) (list s2))]
              [else
               (pathSolve k (+ i 1) next-v1 next-path1 next-v2 next-path2)]))]))])
      (pathSolve (length emission) 1 0 `() 0 `()))))

(viterbiPath '(1 0 1) 0.45 'Fair 'Biased 0.5 0.55)
;=> ((Fair Fair Fair) 0.018906250000000003)
(viterbiPath '(1 1 0 0 1) 0.45 'Fair 'Biased 0.5 0.65)
;=> ((Fair Fair Biased Biased Fair) 0.0016175478515625004)

(viterbiPath '(1 0 0 0 1) 0.45 'Fair 'Biased 0.5 0.65)
;=> ((Fair Biased Biased Biased Fair) 0.002102812207031251)

(viterbiPath '(1) 0 'Fair 'Biased 0 1)
;=> ((Fair) 0.5)
(viterbiPath '(1) 0.45 'Fair 'Biased 0.5 0.65)
;=> ((Fair) 0.25)
(viterbiPath '(1 1 1) 0.5 'Fair 'Biased 1 0)
;=> ((Biased Biased Biased) 0.125)
(viterbiPath '(1 1 1) 0 'Fair 'Biased 0 1)
;=> ((Fair Fair Fair) 0.5)