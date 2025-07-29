;; [[file:../exercise-64.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/assert-tool.rkt"
           "modules/stream-series.rkt"
           "modules/stream-iterator.rkt")
;; Solution:1 ends here

;; [[file:../exercise-64.org::*Testing][Testing:1]]
(assert (< (- (stream-limit (accelerated-sequence euler-transform pi-stream) 1) 3.166) 0.01)
        "testing basic difference")
(assert (> (- (stream-limit (accelerated-sequence euler-transform pi-stream) 1) 3.166) 0.0000000001)
        "testing that we are not over calculating")
(assert (< (- (stream-limit (accelerated-sequence euler-transform pi-stream) 0.0000000000001) 3.14159265358979) 0.00000000000001)
        "testing more difficult difference")
;; Testing:1 ends here
