#lang racket

(define (longest-ascending-sub xs)
  (define (helper l sub result)
    (cond
      [(null? l) (if (> (length sub) (length result))
                     sub
                     result
                     )
                 ]
      [(<= (last sub) (car l)) (helper (cdr l) (append sub (list (car l))) result)]
      [else (if (> (length sub) (length result))
                (helper (cdr l) (list (car l)) sub)
                (helper (cdr l) (list (car l)) result)
                )
            ]
      )
    )

  (helper (cdr xs) (list (car xs)) (list (car xs)))
  )

(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))