#lang racket

(define (reverse x)
  (define (helper result leftover)
    (if (zero? leftover)
        result
        (helper (+ (* result 10) (remainder leftover 10)) (quotient leftover 10))
        )
    )

  (helper 0 x)
  )

(define (num-palindromes-rec first second)
  (cond
    [(> first second) (num-palindromes-rec second first)]
    [(= first second (reverse first)) 1]
    [(= first second) 0]
    [(= (reverse first) first) (add1 (num-palindromes-rec (add1 first) second))]
    [else (num-palindromes-rec (add1 first) second)]
    )
  )

(define (num-palindromes-iter first second)
  (define (helper counter left bigger)
    (cond
      [(> left bigger) counter]
      [(= (reverse left) left) (helper (add1 counter) (add1 left) bigger)]
      [else (helper counter (add1 left) bigger)]
      )
    )

  (if (or (negative? first) (negative? second))
      (error "Negative number in input")
      (helper 0 (min first second) (max first second))
      )
  )

(= (num-palindromes-rec 1 101) 19)
(= (num-palindromes-rec 1 100) 18)
(= (num-palindromes-rec 100 1) 18)

(= (num-palindromes-iter 1 101) 19)
(= (num-palindromes-iter 1 100) 18)
(= (num-palindromes-iter 100 1) 18)