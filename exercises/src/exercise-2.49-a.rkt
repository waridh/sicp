;; [[file:../chapter-2.org::*Part a][Part a:1]]
(#lang sicp)
(#%require sicp-pict)
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))
;; Part a:1 ends here
