#lang racket

;Task1
(define (cross-water-min-risk risks)
  (define (helper i)
    (if(< i 0)
       0
       (min (+ (list-ref risks i) (helper (sub1 i)))
            (+ (if(< (sub1 i) 0)
                  0
                  (list-ref risks (sub1 i))
                  )
               (helper (- i 2))
               )
            )
       )
    )
  (helper (sub1 (length risks)))
  )

(equal? (cross-water-min-risk '(10 15 20)) 15)
(equal? (cross-water-min-risk '(1 100 1 1 1 100 1 1 100 1)) 6)
(equal? (cross-water-min-risk '(458 896 809 929 430 241)) 1697)
(equal? (cross-water-min-risk '(945 726 301 716 642 149)) 1591)
(equal? (cross-water-min-risk '(192 31 533 573 772 31)) 635)
(equal? (cross-water-min-risk '(734 401 273 823 715 216 960 474 91 568)) 2005)
(equal? (cross-water-min-risk '(793 413 20 210 963 733 992 500 660 43)) 1899)


;Task2
(define (ith-manhattan cable1 cable2)
  (define (command-L matrix distance row col)
    (if(< (- col distance) 0)
       (command-L (map (λ (x) (string-append (make-string (- distance col) #\.) x)) matrix) distance row col)
       (list-update matrix row (string-append (substring (index-of matrix row) 0 (- col distance 1)) (make-string (- col distance) #\-) (substring (index-of matrix row) (+ col distance 1))))
      )
    )

  (define (command-R matrix distance row col)
    (if(> (+ col distance) (string-length (car matrix)))
       (command-R (map (λ (x) (string-append x (make-string (- distance col) #\.))) matrix) distance row col)
       (list-update matrix row (string-append (substring (index-of matrix row) 0 (sub1 col)) (make-string (- col distance) #\-) (substring (index-of matrix row) (+ col distance 1))))
      )
    )

  (define (command-U matrix distance row col counter)
    (cond
      [(and (zero? distance) (zero? counter)) matrix]
      [(< counter 0) (command-U (append (make-string (string-length (car matrix)) #\.) matrix) distance row col (add1 counter))]
      [else (command-U (list-update matrix row (string-append (substring (index-of matrix row) 0 (sub1 col)) "|" (substring (index-of matrix row) (add1 col)))) (sub1 distance) row col) 0]
      )
    )

  (define (command-D matrix distance row col counter)
    (cond
      [(and (zero? distance) (zero? counter)) matrix]
      [(< (length matrix) counter) (command-D (append matrix (make-string (string-length (car matrix)) #\.)) distance row col (sub1 counter))]
      [else (command-D (list-update matrix row (string-append (substring (index-of matrix row) 0 (sub1 col)) "|" (substring (index-of matrix row) (add1 col)))) (sub1 distance) row col) 0]
      )
    )

  (define (return-result result i)
    (if(> (length result) i)
       (error "Do not have i-th error!")
       (first (drop (sort result <) (sub1 i)))
       )
    )

  (define (helper2 commands result matrix row col startRow startCol i)
    (cond
      [(null? commands) (return-result result i)]
      [(equal? (string-ref (car commands) 0) #\L) (helper2 (cdr commands) (check-L row col (string->number (substring (car commands) 1)) startRow startCol) matrix row (max (- col (string->number (substring (car commands) 1))) 0) startRow startCol i)]
      [(equal? (string-ref (car commands) 0) #\R) (helper2 (cdr commands) (check-R row col (string->number (substring (car commands) 1)) startRow startCol) matrix row (max (+ col (string->number (substring (car commands) 1))) (string-length (car matrix))) startRow startCol i)]
      [(equal? (string-ref (car commands) 0) #\U) (helper2 (cdr commands) (check-U row col (string->number (substring (car commands) 1)) startRow startCol) matrix (max (- row (string->number (substring (car commands) 1))) 0) col startRow startCol i)]
      [(equal? (string-ref (car commands) 0) #\D) (helper2 (cdr commands) (check-D row col (string->number (substring (car commands) 1)) startRow startCol) matrix (max (+ row (string->number (substring (car commands) 1))) (string-length (car matrix))) startRow startCol i)]
      [else (error "Invalid input!")]
      )
    )
  
  (define (helper commands matrix row col startRow startCol i)
    (cond
      [(null? commands) (helper2 (string-split cable2 ",") '() matrix row col startRow startCol i)]
      [(equal? (string-ref (car commands) 0) #\L) (helper (cdr commands) (command-L matrix (string->number (substring (car commands) 1)) row col) row (max (- col (string->number (substring (car commands) 1))) 0) startRow
                                            (if(< (- col (string->number (substring (car commands) 1))) 0)
                                               (+ startCol (- col (string->number (substring (car commands) 1))))
                                               startCol) i)]
      [(equal? (string-ref (car commands) 0) #\R) (helper (cdr commands) (command-R matrix (string->number (substring (car commands) 1)) row col) row (+ col (string->number (substring (car commands) 1))) startRow startCol i)]
      [(equal? (string-ref (car commands) 0) #\U) (helper (cdr commands) (command-U matrix (string->number (substring (car commands) 1)) row col (min (- row (string->number (substring (car commands) 1))) 0)) (max (- row (string->number (substring (car commands) 1))) 0) col
                                                    (if(< (- row (string->number (substring (car commands) 1))) 0)
                                                       (+ startRow (- col (string->number (substring (car commands) 1))))
                                                       startRow) startCol i)]
      [(equal? (string-ref (car commands) 0) #\D) (helper (cdr commands) (command-D matrix (string->number (substring (car commands) 1)) row col (max (length matrix) (+ row (string->number (substring (car commands) 1))))) (+ row (string->number (substring (car commands) 1))) col startRow startCol i)]
      [else (error "Invalid input!")]
      )
    )

  (λ (i) (helper (string-split cable1 ",") '("o") 0 0 0 0 i))
  )



(equal? ((ith-manhattan "R8,U5,L5,D3" "U7,R6,D4,L4") 1) 6)
(equal? ((ith-manhattan "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83") 1) 159)
(equal? ((ith-manhattan "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83") 2) 166)
(equal? ((ith-manhattan "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") 1) 135)
(equal? ((ith-manhattan "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") 3) 158)
(equal? ((ith-manhattan "R998,U367,R735,U926,R23,U457,R262,D473,L353,U242,L930,U895,R321,U683,L333,U623,L10005" "L998,U949,R912,D186,R359,D694,L878,U542,L446,D118,L927,U175,R434,U473,R147,D54,R896,U8890") 1) 4221)