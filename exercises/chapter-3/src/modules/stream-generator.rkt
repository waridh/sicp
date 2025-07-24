;; [[file:../../stream-generator.org::*Source][Source:1]]
#lang sicp
(#%require "stream-base.rkt"
           "stream-combinator.rkt")
(#%provide ones integers)
;; Source:1 ends here

;; [[file:../../stream-generator.org::*Ones][Ones:1]]
(define ones (cons-stream 1 ones))
;; Ones:1 ends here

;; [[file:../../stream-generator.org::*Integers][Integers:1]]
(define integers (cons-stream 1 (add-streams ones integers)))
;; Integers:1 ends here
