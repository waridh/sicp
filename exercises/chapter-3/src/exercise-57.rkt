;; [[file:../exercise-57.org::*Testing][Testing:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt")
;; Testing:1 ends here

;; [[file:../exercise-57.org::*Testing][Testing:2]]
(define target-fib 15)
(define (display-result identifier-string num-additions)
  (newline)
  (display "number of additions to get ")
  (display identifier-string)
  (display " number ")
  (display (+ target-fib 1))
  (display " is ")
  (display num-additions))
;; Testing:2 ends here

;; [[file:../exercise-57.org::*Memoized testing][Memoized testing:1]]
(define memoized-value 0)
(define (show-add-streams stream-1 stream-2)
  (cons-stream (begin
                 (set! memoized-value (+ memoized-value 1))
                 (+ (stream-car stream-1) (stream-car stream-2)))
               (show-add-streams (stream-cdr stream-1) (stream-cdr stream-2))))
;; Memoized testing:1 ends here

;; [[file:../exercise-57.org::*Memoized testing][Memoized testing:2]]
(define fibs (cons-stream 0 (cons-stream 1 (show-add-streams (stream-cdr fibs) fibs))))

(stream-ref fibs target-fib)
(display-result "memoized Fibonacci" memoized-value)
;; Memoized testing:2 ends here

;; [[file:../exercise-57.org::*Unoptimized testing][Unoptimized testing:1]]
(define unmemo-value 0)
;; Where tail is actually just a lambda that force could be applied on
(define (cons-stream-unop head tail)
  (cons head tail))
(define (stream-cdr-unop stream)
  ((cdr stream)))
(define (stream-ref-unop stream value)
  (define (iter acc tail)
    (if (>= acc value)
        (stream-car tail)
        (iter (+ acc 1) (stream-cdr-unop tail))))
  (iter 0 stream))
(define (show-add-streams-unop stream-1 stream-2)
  (cons-stream-unop (begin
                      (set! unmemo-value (+ unmemo-value 1))
                      (+ (stream-car stream-1) (stream-car stream-2)))
                    (lambda ()
                      (show-add-streams-unop (stream-cdr-unop stream-1) (stream-cdr-unop stream-2)))))
(define fibs-unop
  (cons-stream-unop
   0
   (lambda ()
     (cons-stream-unop 1 (lambda () (show-add-streams-unop (stream-cdr-unop fibs-unop) fibs-unop))))))

(newline)
(stream-ref-unop fibs-unop target-fib)
(display-result "unmemoized Fibonacci" unmemo-value)
;; Unoptimized testing:1 ends here
