;; [[file:../chapter-2.org::*Part b][Part b:1]]
#lang sicp
(#%require sicp-pict)
(define bottom-left (make-vect 0 0))
(define bottom-right (make-vect 1 0))
(define top-left (make-vect 0 1))
(define top-right (make-vect 1 1))
;; Part b:1 ends here

;; [[file:../chapter-2.org::*Part b][Part b:2]]
(define (below painter-1 painter-2)
  (let ([paint-below (rotate90 painter-1)]
        [paint-above (rotate90 painter-2)])
    (lambda (frame) ((rotate270 (beside paint-below paint-above)) frame))))
;; Part b:2 ends here

;; [[file:../chapter-2.org::*Part b][Part b:3]]
(display "einstein")
(newline)
(paint einstein)

(display "below einstein")
(newline)
(paint (below einstein einstein))
;; Part b:3 ends here
