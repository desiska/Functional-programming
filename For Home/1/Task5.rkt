#lang racket

(define (snail h d n)
  (define (helper counter sum)
    (if(>= (+ sum d) h) counter (helper (add1 counter) (+ sum (- d n))))
    )
  (helper 1 0)
  )



(= (snail 3 2 1) 2)
(= (snail 10 3 1) 5)
(= (snail 10 3 2) 8)
(= (snail 100 20 5) 7)
(= (snail 5 10 3) 1)