#lang sicp

(define
  (average x y)
  (/ (+ x y) 2))

(define (abs x) (if (< x 0) (- x) x))

;; Returns a guess that is closer to the sqrt of the value
(define (improve guess value)
  (average guess (/ value guess)))

;; Predicate that returns true if the guess was close enough to the sqrt of
;; the value
(define (close-enough? guess value)
  (< (abs (- value (* guess guess))) 0.001))

(define (sqrt-iter guess value)
  (if (close-enough? guess value)
      guess
      (sqrt-iter (improve guess value) value)))

;; This is the user interface for the function
(define (sqrt value) (sqrt-iter 1.0 value))

(sqrt 9)

;; Here is the edge case that would break the previous version
;; This should be 0.01, but it will evaluate to around 0.03
(sqrt 0.0001)

;; With this many zeros, the line below actually just does not evaluate
;; (sqrt 1000000000000000)

(define (new-close-enough? guess prev-guess)
  (< (abs (- guess prev-guess)) 0.001))

(define (new-sqrt-iter guess prev-guess value)
  (if (new-close-enough? guess prev-guess)
      guess
      (new-sqrt-iter (improve guess value) guess value)))

(define (new-sqrt value) (new-sqrt-iter 1.0 0 value))

;; The following calls will return the correct values
(new-sqrt 9)
(new-sqrt 0.0001)
(new-sqrt 1000000000000000)
