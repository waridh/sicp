;; [[file:../chapter-2.org::*Solution][Solution:1]]
#lang sicp
(#%require sicp-pict)
(define (up-split p n)
  (if (= n 0)
      p
      (let ((up (up-split p (- n 1))))
        (below p (beside up up)))))
(paint (up-split einstein 10))
;; Solution:1 ends here
