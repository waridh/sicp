;; [[file:../exercise-81.org::*Solution][Solution:1]]
#lang sicp
(#%require "./modules/stream-base.rkt")
;; Solution:1 ends here

;; [[file:../exercise-81.org::*Solution][Solution:2]]
;; Using a simple implementation of rand-update for time sake
(define (rand-update x)
  (display "rand-update sees: ")
  (display x)
  (newline)
  (random 100.0))

(define (list->stream l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l) (list->stream (cdr l)))))

(define (rand-stream s)
  (define (make-rand-stream the-stream last-value)
    (if (stream-null? the-stream)
        the-empty-stream
        (let ([head (stream-car the-stream)])
          (cond
            [(and (symbol? head) (eq? head 'generate))
             (let ([ret-value (rand-update last-value)])
               (cons-stream ret-value (make-rand-stream (stream-cdr the-stream) ret-value)))]
            [(and (pair? head) (eq? (car head) 'reset))
             (let ([new-value (rand-update (cadr head))])
               (cons-stream new-value (make-rand-stream (stream-cdr the-stream) new-value)))]
            [else (error "invalid command: RAND " head)]))))
  (make-rand-stream s 0))
;; Solution:2 ends here

;; [[file:../exercise-81.org::*Testing][Testing:1]]
(define input-stream (list->stream '(generate generate (reset 8) generate)))
(define out-stream (rand-stream input-stream))
(display-stream out-stream)
;; Testing:1 ends here
