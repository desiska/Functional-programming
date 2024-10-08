#lang racket

(define (p x)
  (define (helper n result)
    (if(> n x) result (helper (add1 n) (- (+ result (* n 3)) 2)))
    )

  (helper 1 0)
  )

(= (p 1) 1)
(= (p 2) 5)
(= (p 3) 12)
(= (p 4) 22)
(= (p 5) 35)
(= (p 6) 51)