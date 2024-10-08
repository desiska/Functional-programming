#lang racket

(define (remove-first-occurrence number digit)
  (define (helper n d)
    (cond
      ((zero? n) 0)                    
      ((= (remainder n 10) d) (quotient n 10))  
      (else (+ (* 10 (helper (quotient n 10) d)) (remainder n 10)))))

  (helper number digit))


(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)