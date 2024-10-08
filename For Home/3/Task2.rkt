#lang racket

(define (find-max n)
  (define (helper leftover curr-max)
    (if (zero? leftover)
      curr-max
      (helper (quotient leftover 10) (max curr-max (remainder leftover 10)))
      )
    )
  (helper (quotient n 10) (remainder n 10))
  )

(define (remove-first-occurrence number digit)
  (define (helper n d)
    (cond
      ((zero? n) 0)
      ((= (remainder n 10) d) (quotient n 10))  
      (else (+ (* 10 (helper (quotient n 10) d)) (remainder n 10)))))

  (helper number digit))

(define (count-digits n)
  (if (zero? n) 0
      (+ 1 (count-digits (quotient n 10)))
      )
  )

(define (sort-n number)
  (define (count-digits n)
  (if (zero? n) 0
      (+ 1 (count-digits (quotient n 10)))
      )
  )
  (define (helper n sort)
    (cond
      [(and (zero? n) (> (count-digits number) (count-digits sort))) (* sort 10)]
      [(zero? n) sort]
      [else (helper(remove-first-occurrence n (find-max n)) (+ (* sort 10) (find-max n)))]
      )
    )

  (helper number 0)
  )


(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)