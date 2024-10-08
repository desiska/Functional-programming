#lang racket


;Task 1
(define (bouncing-ball height bounce window)
  (define (helper new-height)
    (if(<= new-height window)
       0
       (+ 2 (helper(* new-height bounce)))
      )
    )

  (if(or (negative? height) (or (negative? window) (>= window height)) (and (< 1 bounce) (negative? bounce)))
         -1
         (add1 (helper (* height bounce))))
         ;By description window is lower by height and always will have at least 1
  )

(= (bouncing-ball 3 0.66 1.5) 3)
(= (bouncing-ball 30 0.66 1.5) 15)
(= (bouncing-ball 10 0.6 10) -1)
(= (bouncing-ball 2 0.5 1.0) 1)


;Task2
(define (num-apples year span)

  (define (helper counter apples coef)
    (if (zero? counter)
        apples
        (helper (sub1 counter) (floor (+ apples (* coef 900))) (* coef 0.8))
        )
    )
   
  #|(define (counting-apples counter apples)
    (if (zero? counter)
        apples
        (counting-apples (sub1 counter) (floor (+ (* apples 0.8) 900)))
      )
   )|#

  (if (or (negative? year) (negative? span))
      (error "Invalid input!")
      ;(counting-apples (min year span) 0)
      (helper (min year span) 0 1)
     )
  )

    


(num-apples 2 1)   ; 1
(num-apples 4 8)   ; 2.95
(num-apples 74 10) ; 4.426666666666667
(num-apples 1 15)  ; 1
(num-apples 52 77) ; 4.863333333333333

(= (num-apples 2 1) 900)
(= (num-apples 4 8) 2655)
(= (num-apples 74 10) 3984)
(= (num-apples 1 15) 900)
(= (num-apples 52 77) 4377)