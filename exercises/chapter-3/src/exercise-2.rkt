;; Solution
;; Yay, I love higher order functions. We will make three branches, one for showing the count, one fore resetting the count, and one for evaluating the expression. Our reset method will return the call count, and special symbol application will not count towards the call count.


;; [[file:../exercise-2.org::*Solution][Solution:1]]
#lang sicp

(define (make-monitored f)
  (let ([call-count 0])
    (lambda (x)
      (cond [(and (symbol? x) (eq? x 'how-many-calls?)) call-count]
            [(and (symbol? x) (eq? x 'reset-count)) (begin (set! call-count 0) call-count)]
            [else (begin (set! call-count (+ call-count 1)) (f x))]))))
;; Solution:1 ends here

;; [[file:../exercise-2.org::*Solution][Solution:2]]
(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
;; Solution:2 ends here
