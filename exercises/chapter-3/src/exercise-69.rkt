;; [[file:../exercise-69.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-generator.rkt"
           "modules/stream-combinator.rkt")
;; Solution:1 ends here

;; [[file:../exercise-69.org::*Solution][Solution:2]]
(define (triples-aux si tj+1 uk+1)
  (cons-stream (list (stream-car si) (stream-car tj+1) (stream-car uk+1))
               (interleave (stream-map (lambda (x) (list (stream-car si) (stream-car tj+1) x))
                                       (stream-cdr uk+1))
                           (triples-aux si (stream-cdr tj+1) (stream-cdr uk+1)))))
(define (triples s t u)
  (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
               (interleave (interleave (triples-aux s (stream-cdr t) (stream-cdr u))
                                       (stream-map (lambda (x) (list (stream-car s) (stream-car t) x))
                                                   (stream-cdr u)))
                           (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
;; Solution:2 ends here

;; [[file:../exercise-69.org::*Solution][Solution:3]]
(newline)
(display "first 21 elements of (triples integers integers)")
(newline)
(display-stream-range 0 20 (triples integers integers integers))
;; Solution:3 ends here

;; [[file:../exercise-69.org::*Solution][Solution:4]]
(define (pythagorian-predicate i j k)
  (= (expt k 2) (+ (expt i 2) (expt j 2))))

(define pythagorian-triples
  (stream-filter (lambda (x) (pythagorian-predicate (car x) (cadr x) (caddr x)))
                 (triples integers integers integers)))
;; Solution:4 ends here

;; [[file:../exercise-69.org::*Solution][Solution:5]]
(newline)
(display "8 pythagorian triples")
(newline)
(display-stream-range 0 7 pythagorian-triples)
;; Solution:5 ends here
