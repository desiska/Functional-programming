#lang racket

(define (automorphic? n)
  (define (helper sqrt-num num)
    (cond
      [(zero? num) #t]
      [(= (remainder sqrt-num 10) (remainder num 10)) (helper (quotient sqrt-num 10) (quotient num 10))]
      [else #f]
      )
    )

  (if (or (negative? n) (zero? n))
      (error "n was not natural!")
      (helper (expt n 2) n)
    )
  )

(equal? (automorphic? 3)#f)
(equal? (automorphic? 10)#f)
(equal? (automorphic? 5)#t)
(equal? (automorphic? 25)#t)
(equal? (automorphic? 76)#t) 
(equal? (automorphic? 890625)#t) 
(equal? (automorphic? 625)#t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)
;(automorphic? -1) ; error: n was not natural
;(automorphic? 0) ; error: n was not natural