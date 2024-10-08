#lang racket

(define (rev n)
  (define (helper num result)
    (if (zero? num) result (helper (quotient num 10) (+ (* result 10) (remainder num 10))))
    )

  (helper n 0)
  )

(= (rev 1) 1)
(= (rev 123) 321)
(= (rev 987654321) 123456789)