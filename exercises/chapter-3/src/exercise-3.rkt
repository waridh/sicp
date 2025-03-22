;; Solution
;; :PROPERTIES:
;; :header-args:racket: :tangle ./src/exercise-3.rkt
;; :END:


;; [[file:../exercise-3.org::*Solution][Solution:1]]
#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password . x)
    "Incorrect password")
  (lambda (p m)
    (if (not (and (symbol? p) (eq? p password)))
        incorrect-password
        (cond [(eq? m 'withdraw)  withdraw]
          [(eq? m 'deposit) deposit ]
          [else (error "Unknown request: MAKE-ACCOUNT" m)])))
  )
;; Solution:1 ends here

;; [[file:../exercise-3.org::*Solution][Solution:2]]
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-passwordf 'deposit) 50)
;; Solution:2 ends here
