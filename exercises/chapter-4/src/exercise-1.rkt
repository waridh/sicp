;; [[file:../exercise-1.org::*Solution][Solution:2]]
#lang sicp
;; Solution:2 ends here

;; [[file:../exercise-1.org::*Solution][Solution:3]]
(define (list-of-values-aux constructor exps env)
  (if (no-operand? exps)
      '()
      (constructor (lambda () (eval (first-operand exps) env))
                   (lambda () (list-of-values-aux (rest-operand exps) env)))))

(define (list-of-values-left exps env)
  (list-of-values-aux (lambda (lhead lrest) (let ((ehead (lhead)))
                                              (cons ehead (lrest))))
                      exps env))

(define (list-of-values-right exps env)
  (list-of-values-aux (lambda (lhead lrest) (let ((erest (lrest)))
                                              (cons (lhead) erest)))
                      exps env))
;; Solution:3 ends here

;; [[file:../exercise-1.org::*Testing][Testing:1]]
(stream-ref (estimate-pi) 10000000)
;; Testing:1 ends here
