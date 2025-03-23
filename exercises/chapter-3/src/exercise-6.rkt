;; Implementation
;; :PROPERTIES:
;; :header-args:racket: :exports code :tangle ./src/exercise-6.rkt  :mkdirp yes :comments both
;; :END:


;; [[file:../exercise-6.org::*Implementation][Implementation:1]]
#lang sicp
;; Implementation:1 ends here

;; [[file:../exercise-6.org::*Implementation][Implementation:2]]
(define random-init 0)

(define rand
  (let ([x random-init])
    (lambda (cmd)
      (cond [(and (symbol? cmd) (eq? cmd 'generate)) (begin (set! x (rand-update x)) x)]
            [(and (symbol? cmd) (eq? cmd 'reset)) (lambda (y) (set! x y))]
            [else (error "invalid command: RAND" cmd)]))))
;; Implementation:2 ends here

;; [[file:../exercise-6.org::*Implementation][Implementation:3]]
(define (rand-update x)
  (display "rand-update sees: ") (display x) (newline)
  (random 100.0))
;; Implementation:3 ends here

;; Testing
;; :PROPERTIES:
;; :header-args:racket: :exports code :tangle ./src/exercise-6.rkt  :mkdirp yes :comments both
;; :END:


;; [[file:../exercise-6.org::*Testing][Testing:1]]
(rand 'generate)
(rand 'generate)
((rand 'reset) 8)
(rand 'generate)
;; Testing:1 ends here
