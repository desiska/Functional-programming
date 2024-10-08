#lang racket

(define (insert-at x i xs)
  (define (insert l1 l2 counter)
    (if (zero? counter)
        (append l1 (list x) l2)
        (insert (append l1 (list (car l2))) (cdr l2) (sub1 counter))
        )
    )
  
  (if(or (negative? i) (> i (length xs)))
     (error "Invalid index!")
     (insert '() xs i)
     )
  )

(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 7 0 '(1 2 3)) '(7 1 2 3))
(equal? (insert-at 7 1 '(1 2 3)) '(1 7 2 3))
(equal? (insert-at 7 3 '(1 2 3)) '(1 2 3 7))
(insert-at 7 4 '(1 2 3)) ; error: Invalid index!