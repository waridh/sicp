;; Implementation
;; :PROPERTIES:
;; :header-args:racket: :exports code :tangle ./src/exercise-8.rkt  :mkdirp yes :comments both
;; :END:


;; [[file:../exercise-8.org::*Implementation][Implementation:1]]
#lang sicp
;; Implementation:1 ends here

;; [[file:../exercise-8.org::*Implementation][Implementation:2]]
;; Let 'sentinel be the sentinel value.
(define (make-latch)
  (let ([internal 'sentinel])
    (lambda (x)
      (if (and (symbol? internal) (eq? internal 'sentinel))
          (begin (set! internal x) internal)
          internal))))
;; Implementation:2 ends here

;; Testing
;; :PROPERTIES:
;; :header-args:racket: :exports code :tangle ./src/exercise-8.rkt  :mkdirp yes :comments both
;; :END:


;; [[file:../exercise-8.org::*Testing][Testing:1]]
(define f (make-latch))
(+ (f 0) (f 1))
;; Testing:1 ends here
