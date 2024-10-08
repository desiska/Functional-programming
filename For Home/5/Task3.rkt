#lang racket

(define (rev-fold l)
  (foldr (λ (x y) (+ x (* y 10))) 0 l)
  )

(define (rev-lin-iter l)
  (define (helper result digits x)
    (if(null? x)
       result
       (helper (+ result (* (car x) digits)) (* digits 10) (cdr x))
       )
    )

  (helper 0 1 l)
  )

; using folding
(= (rev-fold '(1 2 3)) 321)
(= (rev-fold '(1 2 3 4 5 6 7 8 9)) 987654321)

; using a linearly iterative procedure
(= (rev-lin-iter '(1 2 3)) 321)
(= (rev-lin-iter '(1 2 3 4 5 6 7 8 9)) 987654321)