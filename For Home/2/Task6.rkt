#lang racket
(require math/number-theory)

 (define (contains-digit? k d)
    (cond
      [(zero? k) #f]
      [(= (remainder k 10) d) #t]
      [else (contains-digit? (quotient k 10) d)]
      )
    )

(define (sum-special-primes n d)
  (define (helper k counter sum)
    (cond
      [(zero? counter) sum]
      [(and (prime? k) (contains-digit? k d)) (helper (add1 k) (sub1 counter) (+ sum k))]
      [else (helper (add1 k) counter sum)]
      )
    )

  (helper 2 n 0)
)

(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)