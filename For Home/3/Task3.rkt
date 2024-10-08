#lang racket

(define (calc-series-sum x n)
  (define (helper result step prev denominator)
    (if(> step n)
       result
       (helper (+ result (/ (* prev -2 x) denominator)) (add1 step) (/ (* prev -2 x) denominator) (+ denominator 2))
       )
    )

  (helper -2 1 -2 3)
  )

;All tests are true. Аn error just pops out from the interval

(= (calc-series-sum 1 0) -2)
(= (calc-series-sum 1 1) -2/3)
(= (calc-series-sum 1 2) -1 1/5)
(= (calc-series-sum 1 3) -1 1/21)
(= (calc-series-sum 1 4) -1 11/135)
(= (calc-series-sum 1 5) -1 29/385)
(= (calc-series-sum 1 6) -1 937/12285)