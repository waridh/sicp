;; [[file:../exercise-30.org::*Solution][Solution:1]]
#lang sicp
;; Solution:1 ends here

;; [[file:../exercise-30.org::*Wire implementation][Wire implementation:1]]
(define (make-wire)
  (let ([action-map '()]
        [local-value 0])
    ;; This local procedure will apply all the stored callbacks
    (define (apply-callbacks callback-tail)
      (if (null? callback-tail)
          'ok
          (begin
            ((car callback-tail))
            (apply-callbacks (cdr callback-tail)))))
    (define (set-value! value)
      (cond
        [(or (= value 1) (= value 0))
         (begin
           (set! local-value value)
           (apply-callbacks action-map))]
        [else (error "unsupported value for a wire" value)]))
    (define (add-callback callback-procedure)
      (set! action-map (cons callback-procedure action-map))
      (callback-procedure))
    (define (dispatch m)
      (cond
        [(eq? m 'get-signal) local-value]
        [(eq? m 'set-signal!) set-value!]
        [(eq? m 'add-action!) add-callback]))
    dispatch))

;; implementation of get-signal
(define (get-signal wire)
  (wire 'get-signal))

;; implementation of set-signal!
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

;; implementation of add-action! Implemented for adding callbacks to
;; changes in the value of the wire objects.
(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))
;; Wire implementation:1 ends here

;; [[file:../exercise-30.org::*Delay][Delay:1]]
;; procedure that implements delay, which is a part of the clocking
;; component of the circuit simulator. Current implementation does not
;; have any real delay
(define (after-delay delay-value callback)
  (callback))
;; Delay:1 ends here

;; [[file:../exercise-30.org::*Primitive gate implementation][Primitive gate implementation:1]]
(define (and-gate a1 a2 output)
  (define (and-action-propagation)
    (let ([new-value (logical-and (get-signal a1) (get-signal a2))])
      (after-delay and-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-propagation)
  (add-action! a2 and-action-propagation)
  'ok)

(define and-gate-delay 0)

(define (logical-and in1 in2)
  (cond
    [(= in1 0) 0]
    [(= in2 0) 0]
    [(and (= in1 1) (= in2 1)) 1]
    [else (error "found invalid input value for and-gate" (list in1 in2))]))

(define (inverter input output)
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond
    [(= s 0) 1]
    [(= s 1) 0]
    [else (error "Invalid signal" s)]))
(define inverter-delay 0)

(define (or-gate a1 a2 output)
  (define (or-action-propagation)
    (let ([new-value (logical-or (get-signal a1) (get-signal a2))])
      (after-delay or-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-propagation)
  (add-action! a2 or-action-propagation)
  'ok)

(define or-gate-delay 0)

(define (logical-or in1 in2)
  (cond
    [(= in1 1) 1]
    [(= in2 1) 1]
    [(and (= in1 0) (= in2 0)) 0]
    [else (error "found invalid input value" (list in1 in2))]))
;; Primitive gate implementation:1 ends here

;; [[file:../exercise-30.org::*Pre-requisite adders][Pre-requisite adders:1]]
(define (half-adder a b s c)
  (let ([d (make-wire)]
        [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ([s (make-wire)]
        [c1 (make-wire)]
        [c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
;; Pre-requisite adders:1 ends here

;; [[file:../exercise-30.org::*Carry Lookahead Adder implementation][Carry Lookahead Adder implementation:1]]
;; This is the ripple carry adder implementation for this exerciser.
(define (ripple-carry-adder a-wires b-wires s-wires c)
  (define (ripple-carry-iter a-tail b-tail s-tail c-prev)
    (cond
      [(or (null? a-tail) (null? b-tail) (null? s-tail))
       (error "input lists have a length mismatch" (list a-wires b-wires s-wires))]
      [(and (null? (cdr a-tail)) (null? (cdr b-tail)) (null? (cdr s-tail)))
       (let ([new-c (make-wire)])
         (begin
           (full-adder (car a-tail) (car b-tail) new-c (car s-tail) c-prev)
           'ok))]
      [else
       (let ([new-c (make-wire)])
         (begin
           (full-adder (car a-tail) (car b-tail) new-c (car s-tail) c-prev)
           (ripple-carry-iter (cdr a-tail) (cdr b-tail) (cdr s-tail) new-c)))]))
  (ripple-carry-iter a-wires b-wires s-wires c))
;; Carry Lookahead Adder implementation:1 ends here

;; [[file:../exercise-30.org::*Testing][Testing:1]]
;; we should write a quick assertion function for this
;; in-val1 will be the value that will be assigned to the wires in
;; a-list, in-val2 is the values that will be assigned to the wires in b-list,
;; and expected-output is the value we expect to retrieved from the s-list.
;; Finally, the expected-carry is the carry value that we expect to retrieve
;; from the system.
(define (assert-ripple-adder in-val1 in-val2 expected-output expected-carry)
  (define (make-corresponding-wires reference-values)
    (if (null? reference-values)
        '()
        (cons (make-wire) (make-corresponding-wires (cdr reference-values)))))
  ;; procedure that will set the values in the wire to match the value list.
  (define (set-wire-values! wires the-values)
    (if (null? wires)
        'ok
        (begin
          (set-signal! (car wires) (car the-values))
          (set-wire-values! (cdr wires) (cdr the-values)))))
  (define (get-wire-values wires)
    (if (null? wires)
        '()
        (cons (get-signal (car wires)) (get-wire-values (cdr wires)))))
  (define (list-=? list-1 list-2)
    (cond
      [(and (null? list-1) (null? list-2)) true]
      [(or (null? list-1) (null? list-2)) false]
      [(not (= (car list-1) (car list-2))) false]
      [else (list-=? (cdr list-1) (cdr list-2))]))
  (define (test-pass?-with-reason result-sum result-carry expected-sum expected-carry)
    (let ([sum-match (list-=? result-sum expected-sum)]
          [carry-match (= result-carry expected-carry)])
      (cond
        [(and sum-match carry-match) (cons true "sum and carry match expected")]
        [(not (or sum-match carry-match)) (cons false "both sum and carry do not match")]
        [(not sum-match) (cons false "sum do not match")]
        [(not carry-match) (cons false "carry does not match")]
        [else (error "unhandled case")])))
  (let ([in-wires1 (make-corresponding-wires in-val1)]
        [in-wires2 (make-corresponding-wires in-val2)]
        [output-wires (make-corresponding-wires in-val1)]
        [carry-out (make-wire)])
    (ripple-carry-adder in-wires1 in-wires2 output-wires carry-out)
    (set-wire-values! in-wires1 in-val1)
    (set-wire-values! in-wires2 in-val2)
    (let ([result-values (get-wire-values output-wires)]
          [result-carry (get-signal carry-out)])
      (let ([test-result
             (test-pass?-with-reason result-values result-carry expected-output expected-carry)])
        (let ([test-bool (car test-result)]
              [test-msg (cdr test-result)])
          (display (if test-bool "pass: " "fail: "))
          (display in-val1)
          (display " + ")
          (display in-val2)
          (display " => ")
          (display expected-output)
          (display " with ")
          (display expected-carry)
          (display " got: ")
          (display result-values)
          (display " with ")
          (display result-carry)
          (newline))))))

(define ripple-carry-tests
  (list (list (list 0) (list 1) (list 1) 0)
        (list (list 1 0) (list 0 1) (list 1 1) 0)
        (list (list 0 1 1 0 0 0 1) (list 0 0 0 0 1 0 0) (list 0 1 1 0 1 0 1) 0)
        (list (list 0 1 1 0 0 0 1) (list 0 0 1 0 1 0 0) (list 1 0 0 0 1 0 1) 0)
        (list (list 1 1) (list 1 0) (list 0 1) 1)
        (list (list 1 1 1 0 0 1) (list 0 0 1 0 0 0) (list 0 0 0 0 0 1) 1)))

(define (apply-ripple-carry-tests tail-tests)
  (if (null? tail-tests)
      'ok
      (let ([curr-test (car tail-tests)])
        (let ([i1 (car curr-test)]
              [i2 (cadr curr-test)]
              [expected-sum (caddr curr-test)]
              [expected-carry (cadddr curr-test)])
          (assert-ripple-adder i1 i2 expected-sum expected-carry)
          (apply-ripple-carry-tests (cdr tail-tests))))))
(apply-ripple-carry-tests ripple-carry-tests)
;; Testing:1 ends here
