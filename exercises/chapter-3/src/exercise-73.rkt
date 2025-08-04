;; [[file:../exercise-73.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt"
           "modules/stream-signal.rkt"
           "modules/assert-tool.rkt")
;; Solution:1 ends here

;; [[file:../exercise-73.org::*Solution][Solution:2]]
(define (RC R C dt)
  (lambda (i v_0)
    (define v
      (add-streams
       (scale-stream (integral i v_0 dt) (/ 1 C))
       (scale-stream i R)))
    v))
;; Solution:2 ends here

;; [[file:../exercise-73.org::*Solution][Solution:3]]
(define RC1 (RC 5 1 0.5))

(newline)
(display "showing RC1 with input of just 1")
(newline)
(display-stream-range 0 100 (RC1 ones 0))
;; Solution:3 ends here

;; [[file:../exercise-73.org::*Solution][Solution:4]]
(define target-test-stream (RC1 ones 0))
(assert (= (stream-ref target-test-stream 0) 5) "expecting 5")
(assert (= (stream-ref target-test-stream 7) 8.5) "expecting 8.5")
(assert (= (stream-ref (RC1 integers 1) 2) 17.5) "expecting (stream-ref (RC1 integers 1) 2) to be 17.5")
;; Solution:4 ends here
