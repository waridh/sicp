#lang sicp
(#%require "stream-base.rkt")
(#%provide assert-list-stream assert-list-infinite-stream
           assert)

(define (assert-list-stream left-list right-sequence)
  (define (pass-print)
    (newline)
    (display "pass: ")
    (display left-list)
    (display " == ")
    (display-stream-range 0 (- (length left-list) 1) right-sequence))
  (define (fail-print msg)
    (newline)
    (display "fail: ")
    (display msg)
    (display "; ")
    (display left-list)
    (display " != ")
    (display-stream-range 0 (- (length left-list) 1) right-sequence))
  (define (recur-check l r)
    (cond
      [(and (null? l) (stream-null? r)) (pass-print)]
      [(or (null? l) (stream-null? r)) (fail-print "unequal length")]
      [(not (equal? (car l) (stream-car r))) (fail-print "found unequal value")]
      [else (recur-check (cdr l) (stream-cdr r))]))
  (recur-check left-list right-sequence))

(define (assert predicate message)
  (newline)
  (if (not predicate) (display "fail: ") (display "pass: "))
  (display message))

(define (assert-list-infinite-stream left-list right-sequence)
  (define (pass-print)
    (newline)
    (display "pass: ")
    (display left-list)
    (display " == ")
    (display-stream-range 0 (- (length left-list) 1) right-sequence))
  (define (fail-print msg)
    (newline)
    (display "fail: ")
    (display msg)
    (display "; ")
    (display left-list)
    (display " != ")
    (display-stream-range 0 (- (length left-list) 1) right-sequence))
  (define (recur-check l r)
    (cond
      [(null? l) (pass-print)]
      [(stream-null? r) (fail-print "stream ended early")]
      [(not (equal? (car l) (stream-car r))) (fail-print "found unequal value")]
      [else (recur-check (cdr l) (stream-cdr r))]))
  (recur-check left-list right-sequence))
