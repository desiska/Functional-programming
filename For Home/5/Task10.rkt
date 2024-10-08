#lang racket

(define (concat-proc l1 l2)
  (append l1 l2)
  )

(define (concat-rec l1 l2)
  (define (merge result leftover)
    (if(null? leftover)
       result
       (merge (cons (car leftover) result) (cdr leftover))
       )
    )

  (reverse (merge (reverse l1) l2))
  )


; using a predefined procedure
(equal? (concat-proc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

; using a linearly iterative process
(equal? (concat-rec '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))