#lang racket

(define (my-gcd x y)
  (
   cond
    [(zero? x) y]
    [(zero? y) x]
    [else (my-gcd (min x y) (remainder (max x y) (min x y)))]
   )
  )

(= (my-gcd 5 13) 1)
(= (my-gcd 13 1235) 13)