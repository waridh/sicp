;; [[file:../../stream-iterator.org::*Source][Source:1]]
#lang sicp
(#%require "stream-base.rkt"
           "stream-combinator.rkt")
(#%provide pi-stream euler-transform stream-limit)
;; Source:1 ends here

;; [[file:../../stream-iterator.org::*Pi][Pi:1]]
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
;; Pi:1 ends here

;; [[file:../../stream-iterator.org::*Euler Transform][Euler Transform:1]]
(define (square x)
  (* x x))
(define (euler-transform s)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)]
                             [s2 (stream-ref s 2)])
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
;; Euler Transform:1 ends here

;; [[file:../../stream-iterator.org::*Stream Limit][Stream Limit:1]]
(define (stream-limit s limit)
  (define (iter prev tail)
    (if (< (abs (- (stream-car tail) prev)) limit)
        (stream-car tail)
        (iter (stream-car tail) (stream-cdr tail))))
  (iter (stream-car s) (stream-cdr s)))
;; Stream Limit:1 ends here
