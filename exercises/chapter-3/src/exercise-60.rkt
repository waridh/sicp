;; [[file:../exercise-60.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-series.rkt"
           "modules/assert-tool.rkt")
;; Solution:1 ends here

;; [[file:../exercise-60.org::*Test][Test:1]]
(assert-list-infinite-stream (list 1 0 -1) (mul-series cosine-series cosine-series))
(assert-list-infinite-stream (list 1 0 0 0 0 0 0)
                             (add-streams (mul-series sine-series sine-series)
                                          (mul-series cosine-series cosine-series)))
;; Test:1 ends here
