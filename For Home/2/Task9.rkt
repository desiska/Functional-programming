#lang racket
(require math/number-theory)

(define (max-multiple d b)
  (define (helper n)
    (cond
      [(zero? n) 0]
      [(divides? d n) n]
      [else (helper (sub1 n))]
      )
    )
    (helper b)
  )

(= (max-multiple 2 7) 6)
(= (max-multiple 3 10) 9)
(= (max-multiple 7 17) 14)
(= (max-multiple 10 50) 50)
(= (max-multiple 37 200) 185)
(= (max-multiple 7 100) 98)