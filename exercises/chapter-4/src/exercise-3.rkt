#lang sicp

(#%require "./modules/data-driven.rkt"
           "./modules/scheme-expressions.rkt"
           "./modules/procedure-base.rkt"
           "./exercise-1.rkt"
           "./modules/environment.rkt")

(define *eval-dd-table* (make-table))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp) (assignment-value exp) env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp) (definition-value exp) env))

(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (my-eval (first-exp exps) env)]
    [else
     (my-eval (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)]))

(define (my-eval exp env)
  (cond
    [(self-evaluating? exp) exp]
    [(variable? exp) (lookup-variable-value exp env)]
    [(quoted? exp) (text-of-quotation exp)]
    [(assignment? exp) (eval-assignment exp env)]
    [(definition? exp) (eval-definition exp env)]
    [(if? exp) (eval-if exp env)]
    [(lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env)]
    [(begin? exp) (eval-sequence (begin-actions exp) env)]
    [(cond? exp) (my-eval (cond->if exp) env)]
    [(application? exp) (apply (my-eval (operator exp) env) (list-of-values (operands exp) env))]
    [else (error "Unknown expression type: EVAL" exp)]))
