#lang racket

(define (upper-bound f y)
  (λ(x) (if(>= y (f x)) y (f x)))
  )

(= ((upper-bound (λ (x) (* 2 x)) 100) 50) 100)
(= ((upper-bound (λ (x) (* 2 x)) 100.236) 500.002) 1000.004)
(= ((upper-bound (λ (x) (* 2 x)) 80) 3) 80)
(= ((upper-bound (λ (x) (* 2 x)) 70) 30) 70)
(= ((upper-bound (λ (x) (* 2 x)) 30) 70) 140)

#|If g is upper-bound (λ (x) (* 2 x)) 100
    then g 50 -> 100
If g is upper-bound (λ (x) (* 2 x)) 100.236
    then g 500.002 -> 1000.004
If g is upper-bound (λ (x) (* 2 x)) 80
    then g 3 -> 80
If g is upper-bound (λ (x) (* 2 x)) 70
    then g 30 -> 70
If g is upper-bound (λ (x) (* 2 x)) 30
    then g 70 -> 140|#