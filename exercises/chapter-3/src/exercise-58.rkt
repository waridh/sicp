;; [[file:../exercise-58.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt")
;; Solution:1 ends here

;; [[file:../exercise-58.org::*Solution][Solution:2]]
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))
;; Solution:2 ends here

;; [[file:../exercise-58.org::*First evaluation][First evaluation:1]]
(define x (expand 1 7 10))
(display "looking at (expand 1 7 10)")
(newline)
(display-stream-range 0 20 x)
;; First evaluation:1 ends here

;; [[file:../exercise-58.org::*First evaluation][First evaluation:2]]
(newline)
(display "(/ 1 7)")
(newline)
(/ 1.0 7.0)
;; First evaluation:2 ends here

;; [[file:../exercise-58.org::*First evaluation][First evaluation:3]]
(define x-prime (expand 8 7 10))
(display "looking at (expand 8 7 10)")
(newline)
(display-stream-range 0 20 x-prime)
;; First evaluation:3 ends here

;; [[file:../exercise-58.org::*Second evaluation][Second evaluation:1]]
(define y (expand 3 8 10))
(newline)
(display "looking at (expand 3 8 10)")
(newline)
(display-stream-range 0 20 y)
;; Second evaluation:1 ends here

;; [[file:../exercise-58.org::*Second evaluation][Second evaluation:2]]
(newline)
(display "(/ 3 8)")
(newline)
(/ 3.0 8.0)
;; Second evaluation:2 ends here

;; [[file:../exercise-58.org::*Radix changes the base][Radix changes the base:1]]
(define y-2 (expand 3 8 2))
(newline)
(display "looking at (expand 3 8 2)")
(newline)
(display-stream-range 0 20 y-2)
;; Radix changes the base:1 ends here

;; [[file:../exercise-58.org::*Radix changes the base][Radix changes the base:2]]
(newline)
(display "(/ 3 8)")
(newline)
(/ 3.0 8.0)
;; Radix changes the base:2 ends here
