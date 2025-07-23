;; [[file:../../stream-combinator.org::*Source][Source:1]]
#lang sicp
(#%require "stream-base.rkt")
(#%provide add-stream)
;; Source:1 ends here

;; [[file:../../stream-combinator.org::*Add stream][Add stream:1]]
(define (add-stream stream-1 stream-2)
  (cons-stream (+ (stream-car stream-1) (stream-car stream-2))
               (add-stream (stream-cdr stream-1) (stream-cdr stream-2))))
;; Add stream:1 ends here
