#lang racket


(define (sum-divisible-numbers start finish k)
  (define (digit-sum n)
    (if (= n 0)
        0
        (+ (remainder n 10) (digit-sum (quotient n 10)))))
  
  (define (helper sum num)
    (cond
      ((> num (max start finish)) sum)
      ((= (remainder (digit-sum num) k) 0)
       (helper (+ sum num) (add1 num)))
      (else (helper sum (add1 num)))))
  
  (helper 0 (min start finish)))

(= (sum-divisible-numbers 0 10 5) 5)
(= (sum-divisible-numbers 0 100 5) 990)
(= (sum-divisible-numbers 100 0 5) 990)