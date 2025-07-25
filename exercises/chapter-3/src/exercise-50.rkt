;; [[file:../exercise-50.org::*Test Bench][Test Bench:1]]
#lang sicp
(#%require "modules/stream-base.rkt" 
           "modules/assert-tool.rkt")
;; Test Bench:1 ends here

;; [[file:../exercise-50.org::*Test Bench][Test Bench:2]]
;; produce a stream of the range [start, end]
(define (generate-range start end)
  (if (> start end)
      the-empty-stream
      (cons-stream start (generate-range (+ start 1) end))))

;; produce a stream of a constant value with the specified size.
(define (generate-const value size)
  (define (maker curr)
    (if (>= curr size)
        the-empty-stream
        (cons-stream value (maker (+ curr 1)))))
  (maker 0))
;; Test Bench:2 ends here

;; [[file:../exercise-50.org::*Test Bench][Test Bench:3]]
(assert-list-stream (list 0 2 4 6 8 10) (stream-map * (generate-range 0 5) (generate-const 2 6)))
;; Test Bench:3 ends here
