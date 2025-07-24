;; [[file:../exercise-54.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt"
           "modules/assert-tool.rkt")
;; Solution:1 ends here

;; [[file:../exercise-54.org::*Factorials][Factorials:1]]
(define factorial
  (cons-stream 1 (mul-streams integers factorial)))
;; Factorials:1 ends here

;; [[file:../exercise-54.org::*Testing][Testing:1]]
(assert-list-infinite-stream (list 1 1 2 6 24 120) factorial)
(assert (= (stream-ref factorial 6) 720) "6! is 720")
;; Testing:1 ends here
