;; [[file:../exercise-37.org::*Solution][Solution:1]]
#lang sicp
;; Solution:1 ends here

;; [[file:../exercise-37.org::*Connectors][Connectors:1]]
(define (make-connector)
  ;; The internal states
  (let ([value false]
        [informant false]
        [constraints '()])
    ;; Setter for the value being held
    (define (set-my-value newval setter)
      (cond
        [(not (has-value? me))
         (set! value newval)
         (set! informant setter)
         (for-each-except setter inform-about-value constraints)]
        [(not (= value newval)) (error "Contradiction" (list value newval))]
        [else 'ignored]))
    ;; Mutator that clears the current value held
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin
            (set! informant false)
            (for-each-except retractor inform-about-no-value constraints))
          'ignored))
    ;; Adding a new constraint if not already registered
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      ;; This will tell the new constraint about the new value
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    ;; Manually setting up self
    (define (me request)
      (cond
        [(eq? request 'has-value?) (if informant true false)]
        [(eq? request 'value) value]
        [(eq? request 'set-value!) set-my-value]
        [(eq? request 'forget) forget-my-value]
        [(eq? request 'connect) connect]
        [else (error "unknown operation: CONNECTOR" request)]))
    me))
;; Connectors:1 ends here

;; [[file:../exercise-37.org::*Connectors][Connectors:2]]
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond [(null? items) 'done]
          [(eq? (car items) exception) (loop (cdr items))]
          [else (procedure (car items))
                (loop (cdr items))]))
  (loop list))
;; Connectors:2 ends here

;; [[file:../exercise-37.org::*Connectors][Connectors:3]]
(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
;; Connectors:3 ends here

;; [[file:../exercise-37.org::*Constraints][Constraints:1]]
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))
;; Constraints:1 ends here

;; [[file:../exercise-37.org::*Constraints][Constraints:2]]
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      [(and (has-value? a1) (has-value? a2)) (set-value! sum (+ (get-value a1) (get-value a2)) me)]
      [(and (has-value? a1) (has-value? sum)) (set-value! a2 (- (get-value sum) (get-value a1)) me)]
      [(and (has-value? a2) (has-value? sum)) (set-value! a1 (- (get-value sum) (get-value a2)) me)]))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      [(eq? request 'I-have-a-value) (process-new-value)]
      [(eq? request 'I-lost-my-value) (process-forget-value)]
      [else (error "Unknown request: ADDER" request)]))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)
;; Constraints:2 ends here

;; [[file:../exercise-37.org::*Constraints][Constraints:3]]
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      [(or (and (has-value? m1) (= (get-value m1) 0)) (and (has-value? m2) (= (get-value m2) 0)))
       (set-value! product 0 me)]
      [(and (has-value? m1) (has-value? m2))
       (set-value! product (* (get-value m1) (get-value m2)) me)]
      [(and (has-value? product) (has-value? m1))
       (set-value! m2 (/ (get-value product) (get-value m1)) me)]
      [(and (has-value? product) (has-value? m2))
       (set-value! m1 (/ (get-value product) (get-value m2)) me)]))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond
      [(eq? request 'I-have-a-value) (process-new-value)]
      [(eq? request 'I-lost-my-value) (process-forget-value)]
      [else (error "Unknown request: MULTIPLIER" request)]))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)
;; Constraints:3 ends here

;; [[file:../exercise-37.org::*Constraints][Constraints:4]]
(define (constant value connector)
  (define (me request)
    (error "unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)
;; Constraints:4 ends here

;; [[file:../exercise-37.org::*Constraints][Constraints:5]]
(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-forget-value)]
          [else (error "unknown request: PROBE" request)]))
  (connect connector me)
  me)
;; Constraints:5 ends here

;; [[file:../exercise-37.org::*Given implementations][Given implementations:1]]
(define (c+ x y)
  (let ([z (make-connector)])
    (adder x y z)
    z))
;; Given implementations:1 ends here

;; [[file:../exercise-37.org::*Our solutions][Our solutions:1]]
(define (c- x y)
  (let ([z (make-connector)])
    (adder z y x)
    z))
;; Our solutions:1 ends here

;; [[file:../exercise-37.org::*Our solutions][Our solutions:2]]
(define (c* x y)
  (let ([z (make-connector)])
    (multiplier x y z)
    z))
;; Our solutions:2 ends here

;; [[file:../exercise-37.org::*Our solutions][Our solutions:3]]
(define (c/ x y)
  (let ([z (make-connector)])
    (multiplier z y x)
    z))
;; Our solutions:3 ends here

;; [[file:../exercise-37.org::*Our solutions][Our solutions:4]]
(define (cv value)
  (let ([x (make-connector)])
    (constant value x)
    x))
;; Our solutions:4 ends here

;; [[file:../exercise-37.org::*Test Bench][Test Bench:1]]
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5)) x) (cv 32)))
;; Test Bench:1 ends here

;; [[file:../exercise-37.org::*Test Bench][Test Bench:2]]
(define (test-c+-forward)
  (let ([a (make-connector)]
        [b (make-connector)])
        (let ([c (c+ a b)])
          (probe "a" a)
          (probe "b" b)
          (probe "a + b" c)
          (constant 1 a)
          (constant 1.3 b))))
(define (test-c--forward)
  (let ([a (make-connector)]
        [b (make-connector)])
        (let ([c (c- a b)])
          (probe "a" a)
          (probe "b" b)
          (probe "a - b" c)
          (constant 1 a)
          (constant 1.3 b))))
(define (test-c*-forward)
  (let ([a (make-connector)]
        [b (make-connector)])
        (let ([c (c* a b)])
          (probe "a" a)
          (probe "b" b)
          (probe "a * b" c)
          (constant 1 a)
          (constant 1.3 b))))
(define (test-c/-forward)
  (let ([a (make-connector)]
        [b (make-connector)])
        (let ([c (c/ a b)])
          (probe "a" a)
          (probe "b" b)
          (probe "a / b" c)
          (constant 1 a)
          (constant 1.3 b))))
(define (test-cv-forward)
  (let ([a (cv 1)]
        [b (make-connector)])
        (let ([c (c+ a b)])
          (probe "a = 1" a)
          (probe "b" b)
          (probe "a + b" c)
          (constant 1.3 b))))

(define (test-1)
  (let ([a (make-connector)])
    (let ([b (celsius-fahrenheit-converter a)])
      (probe "fahrenheit" b)
      (probe "celsius" a)
      (constant -40.0 a))))
(define (test-2)
  (let ([a (make-connector)])
    (let ([b (celsius-fahrenheit-converter a)])
      (probe "fahrenheit" b)
      (probe "celsius" a)
      (constant 8.0 b))))
;; Test Bench:2 ends here

;; [[file:../exercise-37.org::*Test Bench][Test Bench:3]]
(test-c+-forward)
(test-c--forward)
(test-c*-forward)
(test-c/-forward)
(test-cv-forward)
(test-1)
(test-2)
;; Test Bench:3 ends here
