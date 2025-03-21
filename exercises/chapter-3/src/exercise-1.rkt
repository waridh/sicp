;; Solution


;; [[file:../exercise-1.org::*Solution][Solution:1]]
#lang sicp

(define (make-accumulator val)
  (lambda (x) (begin (set! val (+ val x)) val)))
;; Solution:1 ends here

;; [[file:../exercise-1.org::*Solution][Solution:2]]
(define A (make-accumulator 5))
(A 10)
(A 10)
;; Solution:2 ends here
