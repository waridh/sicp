;; [[file:../exercise-68.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt")
;; Solution:1 ends here

;; [[file:../exercise-68.org::*Solution][Solution:2]]
(define (pairs-old s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-old (stream-cdr s) (stream-cdr t)))))
;; Solution:2 ends here

;; [[file:../exercise-68.org::*Solution][Solution:3]]
(define (pairs-louis s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs-louis (stream-cdr s) (stream-cdr t))))
;; Solution:3 ends here

;; [[file:../exercise-68.org::display-old][display-old]]
(newline)
(display "old output, 0 to 20")
(newline)
(display (display-stream-range 0 20 (pairs-old integers integers)))
;; display-old ends here

;; [[file:../exercise-68.org::*Solution][Solution:5]]
(newline)
(display "Louis implementation, 0 to 20")
(newline)
;; (display (display-stream-range 0 20 (pairs-louis integers integers)))
;; Solution:5 ends here
