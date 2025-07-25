;; [[file:../exercise-56.org::*Setup][Setup:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt"
           "modules/assert-tool.rkt")
;; Setup:1 ends here

;; [[file:../exercise-56.org::*Setup][Setup:2]]
(define (merge s1 s2)
  (cond
    [(stream-null? s1) s2]
    [(stream-null? s2) s1]
    [else
     (let ([s1car (stream-car s1)]
           [s2car (stream-car s2)])
       (cond
         [(< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2))]
         [(> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2)))]
         [else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))]))]))
;; Setup:2 ends here

;; [[file:../exercise-56.org::*Implementation][Implementation:1]]
(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))
;; Implementation:1 ends here

;; [[file:../exercise-56.org::*Testing][Testing:1]]
(assert-list-infinite-stream (list 1 2 3 4 5 6 8 9 10 12 15 16 18 20) S)
;; Testing:1 ends here
