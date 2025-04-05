;; Solution
;; :PROPERTIES:
;; :header-args:racket: :tangle ./src/exercise-19.rkt :mkdirp yes :comments both
;; :END:


;; [[file:../exercise-19.org::*Solution][Solution:1]]
#lang sicp
;; Solution:1 ends here

;; [[file:../exercise-19.org::*Solution][Solution:2]]
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
;; Solution:2 ends here



;; We are going to look to use the fast and slow pointer algorithm to traverse the list and find out if there is a cycle. We will use transition between three states every iteration. For two of the three state, we will transition the fast pointer, and for the last state the slow pointer will increment.

;; After every increment, we check if we could return from the state.


;; [[file:../exercise-19.org::*Solution][Solution:3]]
(define (contains? value items)
  (cond [(null? items) false]
        [(eq? (car items) value) true]
        [else (contains? value (cdr items))]))
;; Solution:3 ends here



;; We are going to control the state using an iterative procedure, where the state is an integer.


;; [[file:../exercise-19.org::*Solution][Solution:4]]
(define (cycle? x)
  (let ([slow-pointer nil]
        [fast-pointer x])
    (define (set-pointer! pointer-name value)
      (cond
        [(eq? pointer-name 'slow) (set! slow-pointer value)]
        [(eq? pointer-name 'fast) (set! fast-pointer value)]
        [else (error "illegal pointer name found: " pointer-name)]))
    (define (cycle?-aux state)
      (cond
        [(null? fast-pointer) false]
        [(eq? slow-pointer fast-pointer) true]
        [(or (= state 0) (= state 1))
         (begin
           (set-pointer! 'fast (cdr fast-pointer))
           (cycle?-aux (+ state 1)))]
        [(> state 1)
         (begin
           (set-pointer! 'slow (cdr slow-pointer))
           (cycle?-aux 0))]
        [(< state 0)
         (begin
           (set-pointer! 'slow fast-pointer)
           (set-pointer! 'fast (cdr fast-pointer))
           (cycle?-aux 0))]))
    (cycle?-aux -1)))
;; Solution:4 ends here



;; Now we will have a look at some simple tests.


;; [[file:../exercise-19.org::*Solution][Solution:5]]
(define basic-list (list 'a 'b 'c))
(define basic-cycle (make-cycle (list 'a 'b 'c)))
(define cycle-2 (cons 'x (cons 'y (cons 'z basic-cycle))))

(define (printout-cases name test-value)
  (display "testing against name: ")
  (display name)
  (newline)
  (display "is a cycle?: ")
  (display (cycle? test-value))
  (newline)
  (newline))

(printout-cases "basic-list" basic-list)
(printout-cases "basic-cycle" basic-cycle)
(printout-cases "cycle-2" cycle-2)

;; I think these tests are too easy for something that is
;; supposed to be quick. Time to build a more complicated case.

(define (make-big-list num)
  (define (iter acc curr-count)
    (if (< curr-count 1)
        acc
        (iter (cons curr-count acc) (- curr-count 1))))
  (iter nil num))

(printout-cases "very large value, not a cycle" (make-big-list 10000000))
(printout-cases "very large cycle" (make-cycle (make-big-list 10000000)))
;; Solution:5 ends here
