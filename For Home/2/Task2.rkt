#lang racket

(define (sum-digits-iter n)
  (define (helper counter num)
    (if(zero? num)
       counter
       (helper (+ counter (remainder num 10)) (quotient num 10))
      )
    )

  (if(negative? n)
     (error "n was negative")
     (helper 0 n)
     )
  )

(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)
(sum-digits-iter -13) ; error "n was negative"