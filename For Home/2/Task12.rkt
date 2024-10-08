#lang racket

(define (find-sum a b n)
  (define (helper n1 n2 n3 counter)
    (if(= counter n)
       (+ n1 n2 n3)
       (helper n2 n3 (+ n3 (* (expt 2 counter) b)) (add1 counter))
    )
   )

  (helper (+ a b) (+ a b (* 2 b)) (+ a b (* 2 b) (* 4 b)) 3)
  )

(= (find-sum 0 2 10) 3578) ; 510 + 1022 + 2046
(= (find-sum 5 3 5) 174) ; 26 + 50 + 98