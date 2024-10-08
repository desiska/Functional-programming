#lang racket
(require math/number-theory)

(define (factorize n)
  (define (next-prime k)
    (if (prime? k)
        k
        (next-prime (add1 k))
        )
    )
  
  (define (helper prime leftover l start)
    (cond
      [(= leftover 1) l]
      [(< leftover prime) (helper (next-prime (add1 start)) n '() (next-prime (add1 start)))]
      [(divides? prime leftover) (helper prime (quotient leftover prime) (append l (list prime)) start)]
      [else (helper (next-prime (add1 start)) leftover l (next-prime (add1 start)))]
      )
    )

  (if (prime? n)
      (list n)
      (helper 2 n '() 2)
      )
  )

(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
(equal? (factorize 152) '(2 2 2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))