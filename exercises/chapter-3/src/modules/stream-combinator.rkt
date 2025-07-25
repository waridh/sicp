;; [[file:../../stream-combinator.org::*Source][Source:1]]
#lang sicp
(#%require "stream-base.rkt")
(#%provide add-streams mul-streams combine-streams scale-stream)
;; Source:1 ends here

;; [[file:../../stream-combinator.org::*Generic Combinator][Generic Combinator:1]]
(define (combine-streams proc stream-1 stream-2)
  (cons-stream (proc (stream-car stream-1) (stream-car stream-2))
               (combine-streams proc (stream-cdr stream-1) (stream-cdr stream-2))))
;; Generic Combinator:1 ends here

;; [[file:../../stream-combinator.org::*Scale stream][Scale stream:1]]
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))
;; Scale stream:1 ends here

;; [[file:../../stream-combinator.org::*Add stream][Add stream:1]]
(define (add-streams stream-1 stream-2)
  (combine-streams + stream-1 stream-2))
;; Add stream:1 ends here

;; [[file:../../stream-combinator.org::*Multiply streams][Multiply streams:1]]
(define (mul-streams stream-1 stream-2)
  (combine-streams * stream-1 stream-2))
;; Multiply streams:1 ends here
