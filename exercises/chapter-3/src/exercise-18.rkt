;; Solution
;; :PROPERTIES:
;; :header-args:racket: :tangle ./src/exercise-18.rkt :mkdirp yes :comments both
;; :END:


;; [[file:../exercise-18.org::*Solution][Solution:1]]
#lang sicp
;; Solution:1 ends here



;; We will include the procedure from exercise 3.13 here:


;; [[file:../exercise-18.org::*Solution][Solution:2]]
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
;; Solution:2 ends here



;; We will build our procedure that checks for a cycle to follow the spec, which is to check that we are working with a scheme list, which is singly linked, where the ~car~ points to the content of the cell, and the ~cdr~ is the pointer to the next element of the list, and that the final element of the list will have its ~cdr~ pointing to a ~nil~. I state this because I will be solving this exercise for this specific data structure.

;; The content of the ~car~ is irrelevant, as we don't guarantee that those values are unique. Instead, we will work with the instance of the pairs themselves.

;; Now we have to think about the strategy to detect a cycle. We could use the same strategy as the previous exercise where we keep track of the pairs that we have seen, and if we see that value again, we say that this is a cycle. Now of course the problem with this implementation is that the space complexity of this algorithm grows linearly. We could try to improve this, but that is the next exercise, so maybe we don't do that yet.

;; Once again, we will need a containment check procedure.


;; [[file:../exercise-18.org::*Solution][Solution:3]]
(define (contains? value items)
  (cond [(null? items) false]
        [(eq? (car items) value) true]
        [else (contains? value (cdr items))]))
;; Solution:3 ends here

;; [[file:../exercise-18.org::*Solution][Solution:4]]
(define (cycle? x)
  (let ([seen '()])
    (define (add-seen! value)
      (set! seen (cons value seen)))
    (define (cycle?-aux y)
      (cond
        [(null? y) false]
        [(contains? y seen) true]
        [else
         (begin
           (add-seen! y)
           (cycle?-aux (cdr y)))]))
    (cycle?-aux x)))
;; Solution:4 ends here

;; [[file:../exercise-18.org::*Solution][Solution:5]]
(define basic-list (list 'a 'b 'c))
(define basic-cycle (make-cycle (list 'a 'b 'c)))
(define cycle-2 (cons 'x (cons 'y (cons 'z basic-cycle))))

(define (printout-cases name test-value)
  (display "testing against name: ")
  (display name)
  (newline)
  (display "which has the following shape: ")
  (display test-value)
  (newline)
  (display "is a cycle?: ")
  (display (cycle? test-value))
  (newline)
  (newline))

(printout-cases "basic-list" basic-list)
(printout-cases "basic-cycle" basic-cycle)
(printout-cases "cycle-2" cycle-2)
;; Solution:5 ends here
