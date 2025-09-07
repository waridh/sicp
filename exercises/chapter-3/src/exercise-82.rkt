;; [[file:../exercise-82.org::*Solution][Solution:1]]
#lang sicp
(#%require "./modules/stream-base.rkt"
           "./modules/stream-combinator.rkt")
;; Solution:1 ends here

;; [[file:../exercise-82.org::*Solution][Solution:2]]
;; This is the random-in-range procedure that will provide a random number in provided range
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
;; Solution:2 ends here

;; [[file:../exercise-82.org::*Solution][Solution:3]]
(define (monte-carlo experiment)
  (define (plus-one x)
    (+ x 1))
  (define (make-monte-carlo-stream trials-passed trial-number)
    (if (experiment)
        (cons-stream (/ (plus-one trials-passed) trial-number)
                     (make-monte-carlo-stream (plus-one trials-passed) (plus-one trial-number)))
        (cons-stream (/ trials-passed trial-number)
                     (make-monte-carlo-stream trials-passed (plus-one trial-number)))))
  (make-monte-carlo-stream 0 1))
(define (estimate-integral P x1 x2 y1 y2)
  (let ([total-area (abs (* (- x2 x1) (- y2 y1)))]
        [experi (lambda () (let ([x (random-in-range x1 x2)]
                                 [y (random-in-range y1 y2)])
                             (P x y)))])
    (scale-stream (monte-carlo experi) total-area)))
;; This procedure creates a predicate that checks if the given point is in the circle
(define (make-circle-predicate radius)
  (define (square x)
    (* x x))
  (lambda (x y)
    (<= (+ (square x) (square y)) radius)))

(define (estimate-pi)
  (let ([radius 1.0])
    (scale-stream (estimate-integral (make-circle-predicate radius) 0.0 radius 0.0 radius) 4.0)))
;; Solution:3 ends here

;; [[file:../exercise-82.org::*Testing][Testing:1]]
(stream-ref (estimate-pi) 10000000)
;; Testing:1 ends here
