;; [[file:../exercise-76.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt"
           "modules/assert-tool.rkt")
;; Solution:1 ends here

;; [[file:../exercise-76.org::*Solution][Solution:2]]
;; This is the unit sign change procedure
(define (sign-change-detector new-val prev-val)
  (cond [(and (> new-val 0) (< prev-val 0)) 1]
        [(and (< new-val 0) (> prev-val 0)) -1]
        [else 0]))
;; Solution:2 ends here

;; [[file:../exercise-76.org::*Solution][Solution:3]]
(define (average . x)
  (/ (apply + x) (length x)))
;; The smooth procedure. Allows the user to define what the initial value is
(define (smooth stre init-value)
  (define smoothed-stream (stream-map average stre (cons-stream init-value stre)))
  smoothed-stream)
;; Solution:3 ends here

;; [[file:../exercise-76.org::*Solution][Solution:4]]
(define (make-zero-crossings input-stream init-value)
  (let ([smoothed-stream (smooth input-stream init-value)])
    (stream-map sign-change-detector smoothed-stream (cons-stream init-value smoothed-stream))))
;; Solution:4 ends here

;; [[file:../exercise-76.org::*Solution][Solution:5]]
;; helper procedure that converts list to stream
(define (list->stream l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l) (list->stream (cdr l)))))
;; Solution:5 ends here

;; [[file:../exercise-76.org::*Testing][Testing:1]]
(define (test-stream expected-data s)
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

;; [[file:../exercise-76.org::*Testing][Testing:2]]
(define input-sequence1 (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
(define smoothed-sequence1 (list 0.5 1.5 1.75 1.25 0.75 0.2 -1.05 -2.5 -2.5 -1.25 -0.15 1.6 3.5))
(define expected-sequence1 (list 0 0 0 0 0 0 -1 0 0 0 0 1 0))
;; Testing:2 ends here

;; [[file:../exercise-76.org::*Testing][Testing:3]]
(newline)
(display "Alyssa's implementation")
(test-stream (list->stream expected-sequence1) (make-zero-crossings (list->stream input-sequence1) 0))
;; Testing:3 ends here

;; [[file:../exercise-76.org::*Testing][Testing:4]]
(newline)
(display "Smoothed stream output")
(test-stream (list->stream smoothed-sequence1) (smooth (list->stream input-sequence1) 0))
;; Testing:4 ends here
