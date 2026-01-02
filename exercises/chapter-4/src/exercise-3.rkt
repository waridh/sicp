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
