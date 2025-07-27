;; [[file:../exercise-61.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-series.rkt"
           "modules/assert-tool.rkt")
;; Solution:1 ends here

;; [[file:../exercise-61.org::*Test][Test:1]]
(assert-list-infinite-stream (list 1 0 0 0 0 0 0 0)
                             (mul-series (invert-unit-series cosine-series) cosine-series))
;; Test:1 ends here
