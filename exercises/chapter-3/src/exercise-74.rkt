;; [[file:../exercise-74.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt"
           "modules/assert-tool.rkt")
;; Solution:1 ends here

;; [[file:../exercise-74.org::*Solution][Solution:2]]
(define (stream-from-list l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l)
                   (stream-from-list (cdr l)))))
(define input-sequence (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
(define expected-sequence (list 0 0 0 0 0 -1 0 0 0 0 1 0 0))
(define sense-data (stream-from-list input-sequence))
(define expected-data (stream-from-list expected-sequence))
;; Solution:2 ends here

;; [[file:../exercise-74.org::*Solution][Solution:3]]
(define (sign-change-detector new-val prev-val)
  (cond [(and (> new-val 0) (< prev-val 0)) 1]
        [(and (< new-val 0) (> prev-val 0)) -1]
        [else 0]))
;; Solution:3 ends here

;; [[file:../exercise-74.org::*Solution][Solution:4]]
(define (make-zero-crossings input-stream last-value)
  (if (stream-null? input-stream)
      the-empty-stream
      (cons-stream (sign-change-detector (stream-car input-stream) last-value)
                   (make-zero-crossings (stream-cdr input-stream) (stream-car input-stream)))))
(define alyssa-zero-crossings (make-zero-crossings sense-data 0))
;; Solution:4 ends here

;; [[file:../exercise-74.org::*Solution][Solution:5]]
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))
;; Solution:5 ends here

;; [[file:../exercise-74.org::*Testing][Testing:1]]
(define (test-stream s)
  (define (iter exp-tail res-tail)
    (cond [(and (stream-null? exp-tail) (stream-null? res-tail)) 'done]
          [(stream-null? exp-tail) (begin
                                     (assert false "expected the streams to end at the same time")
                                     'done)]
          [(stream-null? res-tail) (begin
                                      (assert false "input stream ended before expected")
                                      'done)]
          [else (begin (assert (= (stream-car exp-tail)
                                  (stream-car res-tail)) (list '= (stream-car exp-tail)
                                                               (stream-car res-tail)))
                       (iter (stream-cdr exp-tail)
                             (stream-cdr res-tail)))]))
  (iter expected-data s))
;; Testing:1 ends here

;; [[file:../exercise-74.org::*Testing][Testing:2]]
(newline)
(display "Alyssa's implementation")
(test-stream alyssa-zero-crossings)
;; Testing:2 ends here

;; [[file:../exercise-74.org::*Testing][Testing:3]]
(newline)
(display "Our implementation")
(test-stream zero-crossings)
;; Testing:3 ends here
