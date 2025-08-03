;; [[file:../exercise-66.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt"
           "modules/assert-tool.rkt")
;; Solution:1 ends here

;; [[file:../exercise-66.org::*Solution][Solution:2]]
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
;; Solution:2 ends here

;; [[file:../exercise-66.org::*Solution][Solution:3]]
(define pairs-integer-integer (pairs integers integers))
(define (hline)
  (newline)
  (display "--------"))
(newline)
(display "experimental print of the stream from 0 to 20")
(newline)
(display-stream-range 0 100 pairs-integer-integer)
(hline)
;; Solution:3 ends here

;; [[file:../exercise-66.org::*Counter function][Counter function:1]]
(define (count-preceding-pairs target-pair s)
  (let ([first-ele (car target-pair)]
        [second-ele (cadr target-pair)])
    (define (iter acc tail)
      (if (stream-null? tail)
          acc
          (let ([head (stream-car tail)])
            (if (and (= (car head) first-ele) (= (cadr head) second-ele))
                acc
                (iter (+ acc 1) (stream-cdr tail))))))
    (iter 0 s)))
(define (run-bulk-count pairs s)
  (define (run-once pair)
    (newline)
    (display "finding the number of preceding pairs for ")
    (display pair)
    (newline)
    (display (count-preceding-pairs pair s))
    (hline))
  (define (iter tail)
    (if (null? tail)
        'done
        (begin (run-once (car tail))
               (iter (cdr tail)))))
  (iter pairs))
;; Counter function:1 ends here

;; [[file:../exercise-66.org::*Testing][Testing:1]]
(run-bulk-count '((1 100)) pairs-integer-integer)
;; (display "finding the number preceding the pair (99, 100)")
;; (newline)
;; (count-preceding-pairs (list 99 100) pairs-integer-integer)
;; (newline)
;; (hline)
;; (newline)
;; (display "finding the number preceding the pair (100, 100)")
;; (newline)
;; (count-preceding-pairs (list 100 100) pairs-integer-integer)
;; Testing:1 ends here

;; [[file:../exercise-66.org::*Testing][Testing:2]]
(run-bulk-count '((2 100) (3 100) (10 10)) pairs-integer-integer)
;; Testing:2 ends here

;; [[file:../exercise-66.org::*Testing][Testing:3]]
(newline)
(display "testing (3 3)")
(newline)
(count-preceding-pairs (list 3 3) pairs-integer-integer)
(hline)
;; Testing:3 ends here

;; [[file:../exercise-66.org::*Testing][Testing:4]]
(define (fast-expt b n)
  (define (square x) (* x x))
  (define (even? x) (= (remainder x 2) 0))
  (define (fast-expt-iter b n a)
    (if (= n 0) a (if (even? n)
                      (fast-expt-iter (square b) (/ n 2) a)
                      (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))
(define (compute-preceding-pairs pair)
  (let ([first-ele (car pair)]
        [second-ele (cadr pair)])
    (- (* (fast-expt 2 first-ele) (- second-ele (- first-ele 1))) 2)))
(define (bulk-compute-preceding-pairs pair-sequence)
  (define (run-once pair)
    (newline)
    (display "out approx of ") (display pair)
    (newline)
    (display (compute-preceding-pairs pair))
    (hline))
  (define (iter tail)
    (if (null? tail)
        'done
        (begin (run-once (car tail))
               (iter (cdr tail)))))
  (iter pair-sequence))
;; Testing:4 ends here

;; [[file:../exercise-66.org::*Testing][Testing:5]]
(bulk-compute-preceding-pairs (list (list 3 3) (list 2 2) (list 1 1) (list 10 10) (list 1 100) (list 2 100) (list 3 100)))
;; Testing:5 ends here

;; [[file:../exercise-66.org::*Testing][Testing:6]]
(bulk-compute-preceding-pairs '((99 100) (100 100)))
;; Testing:6 ends here
