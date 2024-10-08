#lang racket

(define (count-digits-iter n)
  (define (helper counter num)
    (if(zero? num)
       counter
      (helper (add1 counter) (quotient num 10))
      )
    )

  (if(negative? n)
     (error "n was negative")
     (helper 0 n)
     )
  )

(define (count-digits-rec n)
  (cond
    [(negative? n) (error "n was negative")]
    [(zero? n) 0]
    [else (+ 1 (count-digits-iter (quotient n 10)))]
    )
  )

(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)

(= (count-digits-rec 12345) 5)
(= (count-digits-rec 123) 3)
(count-digits-iter -13) ; error "n was negative"