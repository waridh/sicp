#lang sicp
; Planning:
; If less than or equal to, we will cover the case where they are all equal,
; and the case where there are two smallest
(define (sum-of-square x y) (+ (* x x) (* y y)))
(define (<= x y) (or (< x y) (= x y)))
(define (less-than-others a b c) (and ( <= a b) ( <= a c)))
(define (exercise-1-3 x y z)
  (cond ((less-than-others x y z) (sum-of-square y z))
        ((less-than-others y x z) (sum-of-square x z))
        ((less-than-others z x y) (sum-of-square x y))
        ))

(= (exercise-1-3 1 2 3) 13)
(= (exercise-1-3 3 2 1) 13)
(= (exercise-1-3 2 3 1) 13)
(= (exercise-1-3 3 2 2) 13)
(= (exercise-1-3 2 2 2) 8)
