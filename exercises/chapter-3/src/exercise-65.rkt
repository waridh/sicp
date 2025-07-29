;; [[file:../exercise-65.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-iterator.rkt"
           "modules/stream-combinator.rkt"
           "modules/assert-tool.rkt")
;; Solution:1 ends here

;; [[file:../exercise-65.org::*Implementation][Implementation:1]]
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
              (stream-map - (ln2-summands (+ n 1)))))

;; the base speed of convergence
(define ln2-stream
  (partial-sums (ln2-summands 1)))

;; Euler transform just once
(define ln2-stream2
  (euler-transform ln2-stream))

;; Tableau of euler transform
(define ln2-stream3
  (accelerated-sequence euler-transform ln2-stream))
;; Implementation:1 ends here

;; [[file:../exercise-65.org::*Testing][Testing:1]]
(display-stream-range 0 7 ln2-stream)
(newline)
(display-stream-range 0 7 ln2-stream2)
(newline)
(display-stream-range 0 7 ln2-stream3)
;; Testing:1 ends here
