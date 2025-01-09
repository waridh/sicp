;; [[file:../chapter-2.org::*Solution][Solution:1]]
#lang sicp
(#%require sicp-pict)
(define (split first-proc second-proc)
  (define (proc p n)
    (if (= n 0)
        p
        (let ([rec (proc p (- n 1))]) (first-proc p (second-proc rec rec)))))
  proc)
(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split mark-of-zorro 10))
(paint (up-split einstein 10))
;; Solution:1 ends here
