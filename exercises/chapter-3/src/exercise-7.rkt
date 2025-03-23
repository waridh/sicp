;; Implementation
;; :PROPERTIES:
;; :header-args:racket: :exports code :tangle ./src/exercise-7.rkt  :mkdirp yes :comments both
;; :END:


;; [[file:../exercise-7.org::*Implementation][Implementation:1]]
#lang sicp
;; Implementation:1 ends here

;; Account object change
;; We have a few things that we would like to do.
;; 1. Account objects must have internal management of passwords
;; 2. Account objects must expose a way for more passwords to be added
;; 3. For the sake of debuggability, there should be a getter for the balance held by an account


;; [[file:../exercise-7.org::*Account object change][Account object change:1]]
(define (make-account balance password)
  (let ([passwords (list password)])
    (define (withdraw amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    ;; This is a predicate that will return true if the input password is
    ;; registered with our system.
    (define (password-registered? other-password)
      (define (iter tail)
        (cond
          [(null? tail) false]
          [(eq? other-password (car tail)) true]
          [else (iter (cdr tail))]))
      (if (symbol? other-password)
          (iter passwords)
          false))
    (define (add-password new-password)
      (if (symbol? new-password)
          (set! passwords (cons new-password passwords))
          (error "new password must be a symbol: ADD-PASSWORD" new-password)))
    (define (incorrect-password . x)
      "Incorrect password")
    (lambda (p m)
      (if (not (password-registered? p))
          incorrect-password
          (cond
            [(eq? m 'withdraw) withdraw]
            [(eq? m 'deposit) deposit]
            [(eq? m 'balance) balance]
            [(eq? m 'add-password) add-password]
            [else (error "Unknown request: MAKE-ACCOUNT" m)])))))
;; Account object change:1 ends here

;; Joint account creation

;; [[file:../exercise-7.org::*Joint account creation][Joint account creation:1]]
(define (make-joint account password new-password)
  ((account password 'add-password) new-password)
  account)
;; Joint account creation:1 ends here

;; Testing
;; :PROPERTIES:
;; :header-args:racket: :exports code :tangle ./src/exercise-7.rkt  :mkdirp yes :comments both
;; :END:


;; [[file:../exercise-7.org::*Testing][Testing:1]]
(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 40)
((peter-acc 'some-other-password 'deposit) 50)
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'deposit) 50)
(paul-acc 'rosebud 'balance)
(peter-acc 'rosebud 'balance)
;; Testing:1 ends here
