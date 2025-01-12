;; [[file:../chapter-2.org::*Part a][Part a:1]]
#lang sicp
(#%require sicp-pict)
(define bottom-left (make-vect 0 0))
(define bottom-right (make-vect 1 0))
(define top-left (make-vect 0 1))
(define top-right (make-vect 1 1))
;; Part a:1 ends here

;; [[file:../chapter-2.org::*Part a][Part a:2]]
(define (below painter-1 painter-2)
  (let ([split-point (make-vect 0 0.5)])
    (let ([paint-below (transform-painter painter-1 (make-vect 0 0) (make-vect 1 0) split-point)]
          [paint-above (transform-painter painter-2 split-point (make-vect 1 0.5) (make-vect 0 1))])
      (lambda (frame)
        (paint-below frame)
        (paint-above frame)))))
;; Part a:2 ends here

;; [[file:../chapter-2.org::*Part a][Part a:3]]
(display "einstein")
(newline)
(paint einstein)

(display "below einstein")
(newline)
(paint (below einstein einstein))
;; Part a:3 ends here
