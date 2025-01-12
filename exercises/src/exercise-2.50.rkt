;; [[file:../chapter-2.org::*Solution][Solution:1]]
#lang sicp
(#%require sicp-pict)
(define bottom-left (make-vect 0 0))
(define bottom-right (make-vect 1 0))
(define top-left (make-vect 0 1))
(define top-right (make-vect 1 1))
;; Solution:1 ends here

;; [[file:../chapter-2.org::*Solution][Solution:2]]
(define (flip-horiz painter)
  (transform-painter painter
                     bottom-right
                     bottom-left
                     top-right))
;; Solution:2 ends here

;; [[file:../chapter-2.org::*Solution][Solution:3]]
(display "einstein")
(newline)
(paint einstein)

(display "flipped einstein")
(newline)
(paint (flip-horiz einstein))
;; Solution:3 ends here
