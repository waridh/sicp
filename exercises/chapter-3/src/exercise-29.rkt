#lang sicp

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
      (set! action-map (cons callback-procedure action-map)))
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

;; procedure that implements delay, which is a part of the clocking
;; component of the circuit simulator. Current implementation does not
;; have any real delay
(define (after-delay delay-value callback)
  (callback))

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
  (let ([b1 (make-wire)]
        [b2 (make-wire)]
        [c1 (make-wire)])
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c1)
    (inverter c1 output))
  'ok)

;; we should write a quick assertion function for this

(define (assert-or-gate in-val1 in-val2 expected-output)
  (let ([in-wire1 (make-wire)]
        [in-wire2 (make-wire)]
        [output-wire (make-wire)])
    (let ([or-gate-ut (or-gate in-wire1 in-wire2 output-wire)])
      (set-signal! in-wire1 in-val1)
      (set-signal! in-wire2 in-val2)
      (let ([result-value (get-signal output-wire)])
        (display (if (= expected-output result-value) "pass: " "fail: "))
        (display in-val2)
        (display " | ")
        (display in-val2)
        (display " => ")
        (display expected-output)
        (display " got: ")
        (display result-value)
        (newline)))))

(define or-gate-tests (list (list 0 0 0) (list 0 1 1) (list 1 0 1) (list 1 1 1)))

(define (apply-or-gate-tests tail-tests)
  (if (null? tail-tests)
      'ok
      (let ([curr-test (car tail-tests)])
        (let ([i1 (car curr-test)]
              [i2 (cadr curr-test)]
              [expected (caddr curr-test)])
          (assert-or-gate i1 i2 expected)
          (apply-or-gate-tests (cdr tail-tests))))))
(apply-or-gate-tests or-gate-tests)
