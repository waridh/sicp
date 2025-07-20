;; [[file:../exercise-50.org::*Test Bench][Test Bench:1]]
#lang sicp
(#%require "modules/stream-base.rkt" "modules/stream-map.rkt")
;; Test Bench:1 ends here

;; [[file:../exercise-50.org::*Test Bench][Test Bench:2]]
(define (assert-sequence left-list right-sequence)
  (define (pass-print)
    (newline)
    (display "pass: ")
    (display left-list)
    (display " == ")
    (display-stream right-sequence))
  (define (fail-print msg)
    (newline)
    (display "fail: ")
    (display msg)
    (display "; ")
    (display left-list)
    (display " != ")
    (display-stream right-sequence))
  (define (recur-check l r)
    (cond
      [(and (null? l) (stream-null? r)) (pass-print)]
      [(or (null? l) (stream-null? r)) (fail-print "unequal length")]
      [(not (equal? (car l) (stream-car r))) (fail-print "found unequal value")]
      [else (recur-check (cdr l) (stream-cdr r))]))
  (recur-check left-list right-sequence))
;; Test Bench:2 ends here

;; [[file:../exercise-50.org::*Test Bench][Test Bench:3]]
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
;; Test Bench:3 ends here

;; [[file:../exercise-50.org::*Test Bench][Test Bench:4]]
(assert-sequence (list 0 2 4 6 8 10) (stream-map * (generate-range 0 5) (generate-const 2 6)))
;; Test Bench:4 ends here
