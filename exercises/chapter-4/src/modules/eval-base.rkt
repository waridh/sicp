#lang sicp

(#%require "./scheme-expressions.rkt" "./environment.rkt")
(#%provide eval-assignment eval-definition eval-if eval-sequence)

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp) (assignment-value exp) env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp) (definition-value exp) env))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (eval (first-exp exps) env)]
    [else
     (eval (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)]))
