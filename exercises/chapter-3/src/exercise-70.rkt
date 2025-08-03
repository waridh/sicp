;; [[file:../exercise-70.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-generator.rkt"
           "modules/stream-combinator.rkt"
           "modules/assert-tool.rkt")
;; Solution:1 ends here

;; [[file:../exercise-70.org::*Part A][Part A:1]]
(define (pairs-weighted weight s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (merge-weighted weight
                               (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                               (pairs-weighted weight (stream-cdr s) (stream-cdr t)))))

(define part-a-stream (pairs-weighted (lambda (x) (+ (car x) (cadr x))) integers integers))
;; Part A:1 ends here

;; [[file:../exercise-70.org::*Part A][Part A:2]]
(newline)
(display "first 21 elements of the sorted pairs (i, j), where i <= j")
(newline)
(display (display-stream-range 0 20 part-a-stream))
;; Part A:2 ends here

;; [[file:../exercise-70.org::*Part B][Part B:1]]
(define (divisible a b)
  (= (remainder a b) 0))
(define part-b-stream
  (let ([filtered-stream (stream-filter (lambda (x)
                                          (not (or (divisible x 2) (divisible x 3) (divisible x 5))))
                                        integers)])
    (pairs-weighted (lambda (x) (+ (* 2 (car x)) (* 3 (cadr x)) (* 5 (car x) (cadr x))))
                    filtered-stream
                    filtered-stream)))
;; Part B:1 ends here

;; [[file:../exercise-70.org::*Part B][Part B:2]]
(newline)
(display "part b, first 21 elements")
(newline)
(display-stream-range 0 20 part-b-stream)
;; Part B:2 ends here

;; [[file:../exercise-70.org::*Part B][Part B:3]]
(define (validate-part-b num-tests)
  (define (check-invariant-aux single-pair)
    (define (divisibility-check n)
      (not (or (divisible n 2) (divisible n 3) (divisible n 5))))
    (and (<= (car single-pair) (cadr single-pair))
         (divisibility-check (car single-pair))
         (divisibility-check (cadr single-pair))))
  (define (sorting-weight single-pair)
    (let ([i (car single-pair)]
          [j (cadr single-pair)])
      (+ (* 2 i) (* 3 j) (* 5 i j))))
  (define (check-invariant curr next)
    (and (check-invariant-aux curr)
         (check-invariant-aux next)
         (<= (sorting-weight curr) (sorting-weight next))))
  (define (iter curr-inc tail-stream)
    (if (>= curr-inc num-tests)
        'done
        (begin
          (assert (check-invariant (stream-car tail-stream) (stream-car (stream-cdr tail-stream)))
                  (list (stream-car tail-stream) (stream-car (stream-cdr tail-stream))))
          (iter (+ curr-inc 1) (stream-cdr tail-stream)))))
  (iter 0 part-b-stream))
;; Part B:3 ends here

;; [[file:../exercise-70.org::*Part B][Part B:4]]
(validate-part-b 20)
;; Part B:4 ends here
