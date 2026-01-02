#lang sicp

(#%require "./scheme-expressions.rkt")
(#%provide make-procedure list-of-values)

(define (list-of-values-aux constructor exps env)
  (if (no-operands? exps)
      '()
      (constructor (lambda () (eval (first-operand exps) env))
                   (lambda () (list-of-values-aux (rest-operand exps) env)))))

(define (list-of-values-left exps env)
  (list-of-values-aux (lambda (lhead lrest) (let ([ehead (lhead)]) (cons ehead (lrest)))) exps env))

(define (list-of-values-right exps env)
  (list-of-values-aux (lambda (lhead lrest) (let ([erest (lrest)]) (cons (lhead) erest))) exps env))
;; Solution:3 ends here

(define list-of-values list-of-values-left)

(define (make-procedure parameters body env)
  '())
