;; [[file:../chapter-2.org::*Solution][Solution:2]]
;; Setting up tangle file
#lang sicp
(#%require sicp-pict)
(define bottom-left (make-vect 0 0))
(define bottom-right (make-vect 1 0))
(define top-left (make-vect 0 1))
(define top-right (make-vect 1 1))
;; Solution:2 ends here

;; [[file:../chapter-2.org::*Part a][Part a:1]]
; we just need to make a list that makes square
(define outline-painter (segments->painter (list
                                            (make-segment bottom-left bottom-right)
                                            (make-segment bottom-right top-right)
                                            (make-segment top-right top-left)
                                            (make-segment top-left bottom-left))))

(display "Part a. The outline")
(newline)
(paint outline-painter)
;; Part a:1 ends here

;; [[file:../chapter-2.org::*Part b][Part b:1]]
(define cross-painter (segments->painter (list
                                            (make-segment top-left bottom-right)
                                            (make-segment top-right bottom-left))))

(display "Part b. The cross")
(newline)
(paint cross-painter)
;; Part b:1 ends here

;; [[file:../chapter-2.org::*Part c][Part c:1]]
(define (vector-average . v)
  (define (count-aux x acc)
    (if (null? x)
        acc
        (count-aux (cdr x) (+ acc 1))))
  (define (count x)
    (count-aux x 0))
  (define (vector-sum-aux vecs acc)
    (if (null? vecs)
        acc
        (vector-sum-aux (cdr vecs) (vector-add acc (car vecs)))))
  (define (vector-sum vecs)
    (vector-sum-aux vecs zero-vector))
  (vector-scale (/ 1 (count v)) (vector-sum v)))

(define diamond-painter
  (segments->painter
   (list
    (make-segment (vector-average bottom-left top-left) (vector-average top-left top-right))
    (make-segment (vector-average top-left top-right) (vector-average top-right bottom-right))
    (make-segment (vector-average top-right bottom-right) (vector-average bottom-right bottom-left))
    (make-segment (vector-average bottom-right bottom-left) (vector-average bottom-left top-left)))))

(display "Part c. The diamond")
(newline)
(paint diamond-painter)
;; Part c:1 ends here

;; [[file:../chapter-2.org::*Part d][Part d:1]]
(define wave (segments->painter (list
                                            (make-segment top-left bottom-right)
                                            (make-segment top-right bottom-left))))

(display "Part d. wave")
(newline)
(paint wave)
;; Part d:1 ends here
