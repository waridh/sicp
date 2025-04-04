;; Solution
;; :PROPERTIES:
;; :header-args:racket: :tangle ./src/exercise-17.rkt :mkdirp yes :comments both
;; :END:


;; [[file:../exercise-17.org::*Solution][Solution:1]]
#lang sicp
;; Solution:1 ends here



;; Make a procedure for containment check in a list.


;; [[file:../exercise-17.org::*Solution][Solution:2]]
(define (contains? value items)
  (cond [(null? items) false]
        [(eq? (car items) value) true]
        [else (contains? value (cdr items))]))
;; Solution:2 ends here



;; This should be enough, now we could use a mutating architecture to update the seen list structure. If we see a pair that already has been seen, we will end the recursion, and not count the seen pair.

;; Since we are using a mutating architecture, we have to set up a local variable that we will be mutating for each application of ~count-pairs~. Just to be nice and precise, I will be making a procedure for adding a seen value to the ~seen~ collection.


;; [[file:../exercise-17.org::*Solution][Solution:3]]
(define (count-pairs x)
  (let ([seen '()])
    (define (add-seen! value)
      (set! seen (cons value seen)))
    (define (count-pairs-aux y)
     (cond
        [(or (not (pair? y)) (contains? y seen)) 0]
        [else
         (begin
           (add-seen! y)
           (+ (count-pairs-aux (car y)) (count-pairs-aux (cdr y)) 1))]))
    (count-pairs-aux x)))
;; Solution:3 ends here



;; Now, we are going to test against all the values from [[file:exercise-16.org][exercise 3.16]].


;; [[file:../exercise-17.org::*Solution][Solution:4]]
(define three-pairs-count-three (cons 'a (cons 'b (cons 'c nil))))
(define terminating-pair (cons 'c nil))
(define three-pairs-count-four (cons 'a (cons terminating-pair terminating-pair)))
(define pair-2 (cons terminating-pair terminating-pair))
(define three-pairs-count-seven (cons pair-2 pair-2))
(define looping-1 (cons 'a nil))
(define looping-2 (cons 'b looping-1))
(define looping-3 (cons 'c looping-2))
(set-cdr! looping-1 looping-3)

(define (printout-cases name test-value)
  (display "testing against name: ")
  (display name)
  (newline)
  (display "which has the following shape: ")
  (display test-value)
  (newline)
  (display "number of pairs: ")
  (display (count-pairs test-value))
  (newline)
  (newline))

(printout-cases "three-pairs-count-three" three-pairs-count-three)
(printout-cases "three-pairs-count-four" three-pairs-count-four)
(printout-cases "three-pairs-count-seven" three-pairs-count-seven)
(printout-cases "infinte-loops" looping-1)
;; Solution:4 ends here
