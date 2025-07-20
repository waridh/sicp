;; [[file:../exercise-51.org::*Read execution of code][Read execution of code:1]]
#lang sicp
(#%require "modules/stream-base.rkt" "modules/stream-map.rkt")
(define (show x)
  (display-line x)
  x)
(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
;; Read execution of code:1 ends here
