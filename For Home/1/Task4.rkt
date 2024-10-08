#lang racket

(define (growing-plant up down height)
  (define (helper counter sum)
    (if(>= (+ sum up) height) counter (helper (add1 counter) (+ sum (- up down))))
    )
  (helper 1 0)
  )

(= (growing-plant 5 2 5) 1)
(= (growing-plant 5 2 6) 2)
(= (growing-plant 10 9 4) 1)
(= (growing-plant 100 10 910) 10) ; up-speed=100, down-speed=10, desired-height=910