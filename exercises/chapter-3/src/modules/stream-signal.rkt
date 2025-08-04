;; [[file:../../stream-signal.org::*Source][Source:1]]
#lang sicp
(#%require "stream-base.rkt"
           "stream-combinator.rkt")
(#%provide integral)
;; Source:1 ends here

;; [[file:../../stream-signal.org::*Integral][Integral:1]]
(define (integral integrand initial-value dt)
  (define int
    (cons-stream  initial-value
                  (add-streams (scale-stream integrand dt)
                               int)))
  int)
;; Integral:1 ends here
