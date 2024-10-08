#lang racket
(require math/number-theory)

(define (sum-digits k sum)
    (if (zero? k)
        sum
        (sum-digits (quotient k 10) (+ sum (remainder k 10)))))

(define (interesting? n)  
  (divides? (sum-digits n 0) n)
  )

(equal? (interesting? 410) #t)
(equal? (interesting? 212) #f)
(equal? (interesting? 567) #f)
(equal? (interesting? 70) #t)
(equal? (interesting? 5) #t)
(equal? (interesting? 4) #t)