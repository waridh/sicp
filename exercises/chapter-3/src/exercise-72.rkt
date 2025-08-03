;; [[file:../exercise-72.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt"
           "modules/assert-tool.rkt")
;; Solution:1 ends here

;; [[file:../exercise-72.org::*Solution][Solution:2]]
(define (pairs-weighted weight s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (merge-weighted weight
                               (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                               (pairs-weighted weight (stream-cdr s) (stream-cdr t)))))
;; Solution:2 ends here

;; [[file:../exercise-72.org::*Solution][Solution:3]]
(define (weight-function a b)
  (+ (expt a 2) (expt b 2)))

(define (weight-function-of-list l)
  (weight-function (car l) (cadr l)))

(define (target-transformer s)
  (define (iter acc tail)
    (if (null? acc)
        (iter (list (stream-car tail)) (stream-cdr tail))
        (let ([acc-weight (weight-function-of-list (car acc))]
              [next-weight (weight-function-of-list (stream-car tail))])
          (cond
            [(and (>= (length acc) 3) (not (= acc-weight next-weight)))
             (cons-stream acc (iter '() (stream-cdr tail)))]
            [(>= (length acc) 3) (iter '() (stream-cdr tail))]
            [(= acc-weight next-weight) (iter (cons (stream-car tail) acc) (stream-cdr tail))]
            [else (iter '() (stream-cdr tail))]))))
  (iter '() s))

(define square-sum-triples
  (target-transformer (pairs-weighted weight-function-of-list integers integers)))
;; Solution:3 ends here

;; [[file:../exercise-72.org::*Solution][Solution:4]]
(newline)
(display "showing some triples")
(newline)
(display-stream-range 0 5 square-sum-triples)
;; Solution:4 ends here

;; [[file:../exercise-72.org::*Solution][Solution:5]]
(define (get-square-sums s)
  (cons-stream (cons (weight-function-of-list (car (stream-car s))) (stream-car s))
               (get-square-sums (stream-cdr s))))
;; Solution:5 ends here

;; [[file:../exercise-72.org::*Solution][Solution:6]]
(newline)
(display "triples with sum")
(newline)
(display-stream-range 0 5 (get-square-sums square-sum-triples))
;; Solution:6 ends here

;; [[file:../exercise-72.org::*Solution][Solution:7]]
(define (validate-system num-tests)
  (define (check-invariant-aux acc tail)
    (cond [(null? tail) true]
          [(not (= acc (weight-function-of-list (car tail)))) false]
          [else (check-invariant-aux acc (cdr tail))]))
  (define (check-invariant curr)
    (check-invariant-aux (weight-function-of-list (car curr)) (cdr curr)))
  (define (iter curr-inc tail-stream)
    (if (>= curr-inc num-tests)
        'done
        (begin
          (assert (check-invariant (stream-car tail-stream))
                  (stream-car tail-stream))
          (iter (+ curr-inc 1) (stream-cdr tail-stream)))))
  (iter 0 square-sum-triples))
;; Solution:7 ends here

;; [[file:../exercise-72.org::*Solution][Solution:8]]
(validate-system 20)
;; Solution:8 ends here
