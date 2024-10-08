#lang racket

(define (count-occurrences n d)
    (define (helper k)
    (cond
      [(zero? k) 0]
      [(= (remainder k 10) d) (add1 (helper (quotient k 10)))]
      [else (helper (quotient k 10))]
      )
    )

  (cond
    [(negative? n) (error "Negative number!")]
    [(zero? n) 1]
    [else (helper n)]
    )
  )

(= (count-occurrences 121 1) 2)
(= (count-occurrences 222 1) 0)
(= (count-occurrences 100 0) 2)
(= (count-occurrences 0 0) 1)