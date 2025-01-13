;; [[file:../chapter-2.org::*Solution][Solution:1]]
#lang sicp
(#%require sicp-pict)
(define (up-split p n)
  (if (= n 0)
      p
      (let ((up (up-split p (- n 1))))
        (below p (beside up up)))))
(define (right-split p n)
  (if (= n 0)
      p
      (let ((right (right-split p (- n 1))))
        (beside p (below right right)))))
(define bottom-left (make-vect 0 0))
(define bottom-right (make-vect 1 0))
(define top-left (make-vect 0 1))
(define top-right (make-vect 1 1))
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
;; Solution:1 ends here

;; [[file:../chapter-2.org::*Solution][Solution:2]]
;; Adding new segments to make the painter assymmetrical
(define diamond-painter
  (let ([mid-left (vector-average bottom-left top-left)]
        [top-central (vector-average top-left top-right)]
        [mid-right (vector-average top-right bottom-right)]
        [bottom-central (vector-average bottom-right bottom-left)]
        [middle (make-vect 0.5 0.5)])
    (segments->painter (list (make-segment mid-left top-central)
                             (make-segment top-central mid-right)
                             (make-segment mid-right bottom-central)
                             (make-segment bottom-central mid-left)
                             (make-segment middle (vector-average middle top-central))
                             (make-segment middle (vector-average middle mid-right))
                             (make-segment (vector-average middle mid-right)
                                           (vector-average middle top-central))))))
;; Solution:2 ends here

;; [[file:../chapter-2.org::*Solution][Solution:3]]
;; Adding new segments to make the painter assymmetrical
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ([top (up-split painter (- n 1))])
        (let ([top-right (beside top top)]
              [corner (corner-split painter (- n 1))])
          (beside (below white corner)
                  (below painter top-right))))))
;; Solution:3 ends here

;; [[file:../chapter-2.org::*Solution][Solution:4]]
;; Adding new segments to make the painter assymmetrical
(define (square-limit painter n)
  (let ([cornered-painter (corner-split painter n)])
    (beside (below (rotate180 cornered-painter) (rotate90 cornered-painter))
            (below (rotate270 cornered-painter) cornered-painter))))
;; Solution:4 ends here

;; [[file:../chapter-2.org::*Solution][Solution:5]]
(paint (square-limit diamond-painter 5))
;; Solution:5 ends here
