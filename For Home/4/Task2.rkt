#lang racket

(define (apply-n f n)
  (if (zero? n)
      (λ (x) x)
      (λ (x) (f ((apply-n f (sub1 n)) x)))
    )
  )

(= ((apply-n (λ (x) (* 2 x)) 5) 2) 64)
;We apply 2 * x first to 2,
;then to (2 * 2) and so on.
;Thus, we get:
;(((((2 * 2) * 2) * 2) * 2) * 2)
;((((4 * 2) * 2) * 2) * 2)
;(((8 * 2) * 2) * 2)
;((16 * 2) * 2)
;(32 * 2)
;64
(= ((apply-n (λ (x) (quotient x 10)) 2) 100) 1)