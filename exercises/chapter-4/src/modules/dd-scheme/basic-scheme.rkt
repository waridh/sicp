#lang sicp

;;; This is the basic implementation of the scheme eval procedure.

(#%require "./evaluator-dd.rkt" "../scheme-expressions.rkt" "../procedure-base.rkt")
(#%provide register-basic-scheme)

(define (register-basic-scheme)
  (register-operation 'quote (lambda (exp _) (text-of-quotation exp)))
  (register-operation 'set! (lambda (exp env) (eval-assignment exp env)))
  (register-operation 'define (lambda (exp env) (eval-definition exp env)))
  (register-operation 'if (lambda (exp env) (eval-if exp env)))
  (register-operation 'lambda
                      (lambda (exp env)
                        (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
  (register-operation 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (register-operation 'cond (lambda (exp env) (dd-eval (cond->if exp) env))))
