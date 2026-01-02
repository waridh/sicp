#lang sicp

;;; This module contains the framework required for the data driven
;;; implementation of the scheme eval procedure.

(#%require "../data-directed.rkt"
           "../environment.rkt"
           "../scheme-expressions.rkt"
           "../procedure-base.rkt")
(#%provide register-operation
           get-operation
           eval-assignment
           eval-definition
           eval-if
           eval-sequence
           dd-eval)

;;; We are designing this data directed table to take procedures that take in
;;; the expression that triggered the code path, and the environment.
(define *eval-dd-table* (make-table))

(define (register-operation op proc)
  "procedure that will add a handling procedure for the data directed
  implementation of eval."
  (add-entry *eval-dd-table* op proc))

(define (get-operation op)
  (get-value *eval-dd-table* op))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp) (assignment-value exp) env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp) (definition-value exp) env))

(define (eval-if exp env)
  (if (true? (dd-eval (if-predicate exp) env))
      (dd-eval (if-consequent exp) env)
      (dd-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (dd-eval (first-exp exps) env)]
    [else
     (dd-eval (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)]))

(define (dd-eval exp env)
  (cond
    [(self-evaluating? exp) exp]
    [(variable? exp) (lookup-variable-value exp env)]
    [else
     (let ([lookup-res (get-operation (list-tag exp))])
       (if (not lookup-res)
           (if (application? exp)
               (apply (dd-eval (operator exp) env) (list-of-values (operands exp) env))
               (error "Unknown expression type: EVAL" exp))
           (lookup-res exp env)))]))
