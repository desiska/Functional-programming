#lang racket
(require math/number-theory)

(define (sum-prime-divs-rec n)
  (define (helper k)
    (cond
      [(zero? k) 0]
      [(and (prime? k) (divides? k n)) (+ k (helper (sub1 k)))]
      [else (helper (sub1 k))]
      )
    )

  (helper n)
  )

(= (sum-prime-divs-rec 0) 0)
(= (sum-prime-divs-rec 6) 5) ; 2 + 3
(= (sum-prime-divs-rec 18) 5) ; 2 + 3
(= (sum-prime-divs-rec 19) 19)
(= (sum-prime-divs-rec 45136) 53)