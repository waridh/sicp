;; [[file:../exercise-67.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt")
;; Solution:1 ends here

;; [[file:../exercise-67.org::*Solution][Solution:2]]
(define (pairs s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave (interleave (stream-map (lambda (x) (list (stream-car s) x))
                                                   (stream-cdr t))
                                       (pairs (stream-cdr s) (stream-cdr t)))
                           (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s)))))
;; Solution:2 ends here

;; [[file:../exercise-67.org::*Solution][Solution:3]]
(display-stream-range 0 100 (pairs integers integers))
;; Solution:3 ends here
