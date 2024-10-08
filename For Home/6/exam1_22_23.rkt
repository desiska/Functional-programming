#lang racket
(require math/number-theory)

;Task1
(define (primes-prod x)
  (define (helper n result sqrt-x)
    (cond
      [(> n sqrt-x) result]
      [(prime? n) (helper (add1 n) (* result n) sqrt-x)]
      [else (helper (add1 n) result sqrt-x)]
      )
    )

  (helper 2 1 (sqrt x))
  )

(equal? (primes-prod 12) 6)
(equal? (primes-prod 49) 210)
(equal? (primes-prod 1200) 200560490130)


;Task2
(define (shuffle-merge xs ys)
  (define (helper l1 l2 result)
    (cond
      [(null? l1) (append result l2)]
      [(null? l2) (append result l1)]
      [else (helper l2 (cdr l1) (append result (list (car l1))))]
      )
    )

  (helper xs ys '())
  )

(equal? (shuffle-merge '(1) '()) '(1))
(equal? (shuffle-merge '(3 4 5) '(2)) '(3 2 4 5))
(equal? (shuffle-merge '(3 4 5) '(9 2)) '(3 9 4 2 5))
(equal? (shuffle-merge '(3 2 8) '(5 6 1 9 11)) '(3 5 2 6 8 1 9 11))


;Task3
(define (g-l-sum limit)
  (define (helper n1 n2 )
    (cond
      [(= (+ (gcd n1 n2) (lcm n1 n2)) limit) (cons n1 n2)]
      [(< limit n1) '(1 . 1)]
      [(< n1 n2) (helper n2 1)]
      [else (helper n1 (add1 n2))]
      )
    )

  (helper 1 1)
  )

(equal? (g-l-sum 2) '(1 . 1))
(equal? (g-l-sum 14) '(6 . 4))