#lang sicp

(#%provide self-evaluating?
           variable?
           quoted?
           assignment?
           assignment-variable
           assignment-value
           definition?
           definition-variable
           definition-value
           lambda?
           lambda-parameters
           lambda-body
           make-lambda
           true?
           if?
           if-predicate
           if-consequent
           if-alternative
           make-if
           begin?
           begin-actions
           make-begin
           last-exp?
           first-exp
           rest-exps
           sequence->exp
           application?
           operator
           operands
           no-operands?
           first-operand
           rest-operand
           text-of-quotation
           list-tag
           cond?
           cond-clauses
           cond-else-clause?
           cond-predicate
           cond-actions
           cond->if)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (list-tag exp)
  (if (pair? exp)
      (car exp)
      exp))

(define (self-evaluating? exp)
  (cond
    [(number? exp) true]
    [(string? exp) true]
    [else false]))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameter body)
  (cons 'lambda (cons parameter body)))

(define (true? symb)
  (and (not (eq? symb 'false)) (not (eq? symb '()))))

(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (null? (cdddr exp))
      'false
      (cadddr exp)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cdr exp))
(define (make-begin seq)
  (cons 'begin seq))

(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))

(define (sequence->exp seq)
  (cond
    [(null? seq) seq]
    [(last-exp? seq) (first-exp seq)]
    [else (make-begin seq)]))

(define (application? exp)
  (pair? exp))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))
(define (no-operands? ops)
  (null? ops))
(define (first-operand ops)
  (car ops))
(define (rest-operand ops)
  (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp)
  (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ([head-clause (car clauses)]
            [rest-clauses (cdr clauses)])
        (cond
          [(cond-else-clause? head-clause)
           (if (not (null? rest-clauses))
               (error "EXPAND-CLAUSES: Else is not the last clause.")
               (sequence->exp (cond-actions head-clause)))]
          [else
           (make-if (cond-predicate head-clause)
                    (sequence->exp (cond-actions head-clause))
                    (expand-clauses rest-clauses))]))))
