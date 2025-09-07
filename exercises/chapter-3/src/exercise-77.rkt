;; [[file:../exercise-77.org::*Solution][Solution:1]]
#lang sicp
;; Solution:1 ends here

;; [[file:../exercise-77.org::*Solution][Solution:2]]
;; This segment handles module imports
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt"
           "modules/assert-tool.rkt")
;; Solution:2 ends here

;; [[file:../exercise-77.org::*Solution][Solution:3]]
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ([integrand (force delayed-integrand)])
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay
                                 (stream-cdr integrand))
                               (+ (* dt (stream-car integrand)) initial-value)
                               dt)))))
;; Solution:3 ends here

;; [[file:../exercise-77.org::*Building dependent procedures][Building dependent procedures:1]]
;; This solves a differential equation
(define (solve f y0 dt)
  (letrec ([y (integral (delay
                          dy)
                        y0
                        dt)]
           [dy (stream-map f y)])
    y))

(define euler-stream (solve (lambda (y) y) 1 0.001))
;; Building dependent procedures:1 ends here

;; [[file:../exercise-77.org::*Application of the test][Application of the test:1]]
(newline)
(display "computing Euler's number")
(stream-ref euler-stream 1000)
;; Application of the test:1 ends here
