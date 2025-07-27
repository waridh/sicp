;; [[file:../exercise-59.org::*Part b][Part b:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt"
           "modules/assert-tool.rkt")
;; Part b:1 ends here

;; [[file:../exercise-59.org::*Part b][Part b:2]]
(define (integrate-series s)
  (define (iter acc tail)
    (cons-stream (* (/ 1 (+ acc 1)) (stream-car tail))
                 (iter (+ acc 1) (stream-cdr tail))))
  (iter 0 s))
;; Part b:2 ends here

;; [[file:../exercise-59.org::*Part b][Part b:3]]
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
;; Part b:3 ends here

;; [[file:../exercise-59.org::*Part b][Part b:4]]
(define cosine-series (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
;; Part b:4 ends here

;; [[file:../exercise-59.org::*Test][Test:1]]
(assert-list-infinite-stream (list 1 1 (/ 1 2) (/ 1 6) (/ 1 24)) exp-series)
(assert-list-infinite-stream (list 1 0 (/ -1 2) 0 (/ 1 24)) cosine-series)
(assert-list-infinite-stream (list 0 1 0 (/ -1 6) 0 (/ 1 120)) sine-series)
;; Test:1 ends here
