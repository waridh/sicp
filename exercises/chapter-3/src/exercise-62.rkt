;; [[file:../exercise-62.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/assert-tool.rkt"
           "modules/stream-series.rkt"
           "modules/stream-series.rkt")
;; Solution:1 ends here

;; [[file:../exercise-62.org::*Test][Test:1]]
(assert-list-infinite-stream (list 1 0 0 0 0 0 0) (div-series cosine-series cosine-series))
(assert-list-infinite-stream (list 1 0 0 0 0 0 0) (div-series exp-series exp-series))
(assert-list-infinite-stream (list 0 1 0 (/ 1 3) 0 (/ 2 15) 0 (/ 17 315))
                             (div-series sine-series cosine-series))
;; Test:1 ends here
