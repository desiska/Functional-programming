#lang racket

(define (can-carry? c k w)
  (cond
    [(or (negative? c) (negative? k) (negative? w)) (error "It cannot have a negative number!")]
    [(<= (* c w) k) #t]
    [else #f]
    )
  )



(equal? (can-carry? 5 15 3) #t)
(equal? (can-carry? 1 5 4) #t)
(equal? (can-carry? 13 25 2) #f)
(equal? (can-carry? 24 104.44 21.12) #f)
(equal? (can-carry? 51 34.75 19.852) #f)
(equal? (can-carry? 42 95.11 0.51) #t)

(can-carry? -13 25 2) ; error: The number of products was negative
(can-carry? 13 -25 2) ; error: John's hosting capacity was negative
(can-carry? 13 25 -2) ; error: The weight of a product was negative