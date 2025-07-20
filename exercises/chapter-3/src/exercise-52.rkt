;; [[file:../exercise-52.org::*Checking][Checking:1]]
#lang sicp
(#%require "modules/stream-base.rkt" "modules/stream-map.rkt")
;; Checking:1 ends here

;; [[file:../exercise-52.org::*Checking][Checking:2]]
(define sum 0)
(define (display-sum)
  (newline)
  (display "sum: ")
  (display sum))
(display-sum)
(define (accum x) (set! sum (+ x sum)) sum)
(display-sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(display-sum)
(define y (stream-filter even? seq))
(display-sum)
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
(display-sum)
(newline)
(stream-ref y 7)
(display-sum)
(newline)
(display-stream z)
(display-sum)
;; Checking:2 ends here
