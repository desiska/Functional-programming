#lang racket
(require math/number-theory)

(define (amicable? n1 n2)
  (define (helper n left)
    (cond
      [(zero? left) 0]
      [(divides? left n) (+ left (helper n (sub1 left)))]
      [else (helper n (sub1 left))]
      )
    )

  (if(or (negative? n1) (negative? n2))
     (error "Negative number!")
     (and (= (helper n1 (quotient n1 2)) n2) (= (helper n2 (quotient n2 2)) n1))
     )
  )

(equal? (amicable? 200 300) #f)
(equal? (amicable? 220 284) #t)
(equal? (amicable? 284 220) #t)
(equal? (amicable? 1184 1210) #t)
(equal? (amicable? 2620 2924) #t)
(equal? (amicable? 6232 6368) #t)