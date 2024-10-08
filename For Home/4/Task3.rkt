#lang racket

(define (derive-n f n eps)
  (if (zero? n)
      f
      (λ (x)(/ (- ((derive-n f (sub1 n) eps) (+ x eps)) ((derive-n f (sub1 n) eps) x)) eps))
     )
  )

(= ((derive-n (λ (x) (* 2 x x x)) 3 1e-3) 2) 12.000015203739167)