;; [[file:../exercise-55.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt"
           "modules/assert-tool.rkt")
;; Solution:1 ends here

;; [[file:../exercise-55.org::*Implementation][Implementation:1]]
(define (partial-sums s)
  (define result
    (cons-stream (stream-car s)
                 (add-streams result (stream-cdr s))))
  result)
;; Implementation:1 ends here

;; [[file:../exercise-55.org::*Test][Test:1]]
(assert-list-infinite-stream (list 1 3 6 10 15 21) (partial-sums integers))
(assert-list-infinite-stream (list 1 2 3 4 5 6 7 8 9 10 11 12) (partial-sums ones))
;; Test:1 ends here
