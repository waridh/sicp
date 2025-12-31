#lang sicp

(#%require "./modules/data-directed.rkt"
           "./modules/scheme-expressions.rkt"
           "./modules/procedure-base.rkt"
           "./exercise-1.rkt"
           "./modules/environment.rkt")

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
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (my-eval (first-exp exps) env)]
    [else
     (my-eval (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)]))

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
    [(cond? exp) (my-eval (cond->if exp) env)]
    [(application? exp) (apply (my-eval (operator exp) env) (list-of-values (operands exp) env))]
    [else (error "Unknown expression type: EVAL" exp)]))

(define (register-basic-scheme)
  (register-operation 'quote (lambda (exp env) (text-of-quotation exp)))
  (register-operation 'set! (lambda (exp env) (eval-assignment exp env)))
  (register-operation 'define (lambda (exp env) (eval-definition exp env)))
  (register-operation 'if (lambda (exp env) (eval-if exp env)))
  (register-operation 'lambda
                      (lambda (exp env)
                        (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
  (register-operation 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (register-operation 'cond (lambda (exp env) (my-eval (cond->if exp) env))))

(define (my-eval exp env)
  (cond
    [(self-evaluating? exp) exp]
    [(variable? exp) (lookup-variable-value exp env)]
    [else
     (let ([lookup-res (get-operation (list-tag exp))])
       (if (not lookup-res)
           (if (application? exp)
               (apply (my-eval (operator exp) env) (list-of-values (operands exp) env))
               (error "Unknown expression type: EVAL" exp))
           (lookup-res exp env)))]))
