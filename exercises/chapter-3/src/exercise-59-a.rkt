;; [[file:../exercise-59.org::*Part a][Part a:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt"
           "modules/assert-tool.rkt")
;; Part a:1 ends here

;; [[file:../exercise-59.org::*Part a][Part a:2]]
(define (integrate-series s)
  (define (iter acc tail)
    (cons-stream (* (/ 1 (+ acc 1)) (stream-car tail))
                 (iter (+ acc 1) (stream-cdr tail))))
  (iter 0 s))
;; Part a:2 ends here

;; [[file:../exercise-59.org::*Testing][Testing:1]]
(assert-list-infinite-stream (list 1 (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5)) (integrate-series ones))
(assert-list-infinite-stream (list 1 1 1 1 1 1 1 1) (integrate-series integers))
;; Testing:1 ends here
