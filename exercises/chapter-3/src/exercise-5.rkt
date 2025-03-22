;; Implementation
;; :PROPERTIES:
;; :header-args:racket: :exports code :tangle ./src/exercise-5.rkt  :mkdirp yes :comments both
;; :END:


;; [[file:../exercise-5.org::*Implementation][Implementation:1]]
#lang sicp
;; Implementation:1 ends here

;; Monte Carlo procedure
;; This is the given ~monte-carlo~ procedure provided by SICP. To just reiterate how we use the Monte Carlo simulation, we are running an experiment on a series of random input in a sample space, and then taking the ratio of successful experiment to all experiments to converge on some result.


;; [[file:../exercise-5.org::*Monte Carlo procedure][Monte Carlo procedure:1]]
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      [(= trials-remaining 0) (/ trials-passed trials)]
      [(experiment) (iter (- trials-remaining 1) (+ trials-passed 1))]
      [else (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))
;; Monte Carlo procedure:1 ends here

;; Random in range
;; This is the random in range procedure, also provided to us by SICP


;; [[file:../exercise-5.org::*Random in range][Random in range:1]]
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
;; Random in range:1 ends here

;; Monte Carlo Integration


;; [[file:../exercise-5.org::*Monte Carlo Integration][Monte Carlo Integration:1]]
(define (estimate-integral trials P x1 x2 y1 y2)
  ;; This is the experiment for finding the if a random value would fall in the
  ;; bound that was defined by the predicate. If we were to talk a little about
  ;; what it is that we are doing here, we say that we are making a closure that
  ;; tests out
  (define (expri)
    (let ([x (random-in-range x1 x2)]
          [y (random-in-range y1 y2)])
      (P x y)))
  ;; We could definitely lazily evaluate the total-area
  (define (total-area)
    (abs (* (- x2 x1) (- y2 y1))))
  (* (monte-carlo trials expri) (total-area)))
;; Monte Carlo Integration:1 ends here

;; Circle predicate

;; We are going to go with making a higher order procedure to define our predicate so that we could manage the radius being used from a different scope.


;; [[file:../exercise-5.org::*Circle predicate][Circle predicate:1]]
(define (make-circle-predicate radius)
  (define (square x)
    (* x x))
  (lambda (x y)
    (<= (+ (square x) (square y)) radius)))
;; Circle predicate:1 ends here

;; Estimating Pi
;; Now we just have to put everything together.


;; [[file:../exercise-5.org::*Estimating Pi][Estimating Pi:1]]
(define (estimate-pi trials)
  (let ([radius 1.0])
    (* 4.0 (estimate-integral trials (make-circle-predicate radius) 0.0 radius 0.0 radius))))
;; Estimating Pi:1 ends here

;; Testing
;; :PROPERTIES:
;; :header-args:racket: :exports code :tangle ./src/exercise-5.rkt  :mkdirp yes :comments both
;; :END:


;; [[file:../exercise-5.org::*Testing][Testing:1]]
(estimate-pi 10000000)
;; Testing:1 ends here
