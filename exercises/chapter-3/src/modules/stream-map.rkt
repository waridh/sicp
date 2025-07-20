;; [[file:../../exercise-50.org::*Implementation][Implementation:1]]
#lang sicp
(#%require "stream-base.rkt")
(#%provide stream-map)

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map (cons proc (map stream-cdr argstreams))))))
;; Implementation:1 ends here
