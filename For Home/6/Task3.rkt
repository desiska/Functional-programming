#lang racket

(define (num-bigger-elements xs)
  #|(define (helper result l)
    (if(null? l)
       result
       (helper (append result (list (cons (car l) (length (filter (λ (x) (> x (car l))) xs))))) (cdr l))
       )
    )

  (helper '() xs)|#

  (map (λ (x) (cons x (length (filter (λ (y) (> y x)) xs)))) xs)
  )

(equal? (num-bigger-elements '(5 6 3 4)) '((5 . 1) (6 . 0) (3 . 3) (4 . 2)))
(equal? (num-bigger-elements '(1 1 1)) '((1 . 0) (1 . 0) (1 . 0)))