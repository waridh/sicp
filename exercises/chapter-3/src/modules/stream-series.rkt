;; [[file:../../stream-series.org::*Source][Source:1]]
#lang sicp
(#%require "stream-base.rkt" "stream-combinator.rkt" "stream-generator.rkt")
(#%provide integrate-series exp-series sine-series cosine-series mul-series)
;; Source:1 ends here

;; [[file:../../stream-series.org::*Integrate series][Integrate series:1]]
(define (integrate-series s)
  (define (iter acc tail)
    (cons-stream (* (/ 1 (+ acc 1)) (stream-car tail))
                 (iter (+ acc 1) (stream-cdr tail))))
  (iter 0 s))
;; Integrate series:1 ends here

;; [[file:../../stream-series.org::*Exponential Series][Exponential Series:1]]
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
;; Exponential Series:1 ends here

;; [[file:../../stream-series.org::*Sine and Cosine][Sine and Cosine:1]]
(define cosine-series (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
;; Sine and Cosine:1 ends here

;; [[file:../../stream-series.org::*Multiply Series][Multiply Series:1]]
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))
;; Multiply Series:1 ends here
