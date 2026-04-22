#|
Exercise Prompt

Rewrite eval so that the dispatch is done in data-directed style. Compare
this with the data-directed differentiation procedure of Exercise 2.73. (You may
use the car of a compound expression as the type of the expression, as is
appropriate for the syntax implemented in this section.)

The implementation of this exercise shall be REPL driven
|#

#lang sicp

(#%require "./modules/data-directed.rkt"
           "./modules/scheme-expressions.rkt"
           "./modules/procedure-base.rkt"
           "./modules/environment.rkt"
           "./modules/dd-scheme/basic-scheme.rkt"
           "./modules/dd-scheme/evaluator-dd.rkt")

(register-basic-scheme)

(define (ref-my-eval exp env)
  "the reference implementation of eval"
  (cond
    [(self-evaluating? exp) exp]
    [(variable? exp) (lookup-variable-value exp env)]
    [(quoted? exp) (text-of-quotation exp)]
    [(assignment? exp) (eval-assignment exp env)]
    [(definition? exp) (eval-definition exp env)]
    [(if? exp) (eval-if exp env)]
    [(lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env)]
    [(begin? exp) (eval-sequence (begin-actions exp) env)]
    [(cond? exp) (ref-my-eval (cond->if exp) env)]
    [(application? exp) (apply (ref-my-eval (operator exp) env) (list-of-values (operands exp) env))]
    [else (error "Unknown expression type: EVAL" exp)]))
