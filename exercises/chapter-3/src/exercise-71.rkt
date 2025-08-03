;; [[file:../exercise-71.org::*Solution][Solution:1]]
#lang sicp
(#%require "modules/stream-base.rkt"
           "modules/stream-combinator.rkt"
           "modules/stream-generator.rkt")
;; Solution:1 ends here

;; [[file:../exercise-71.org::*Solution][Solution:2]]
(define (pairs-weighted weight s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (merge-weighted weight
                               (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                               (pairs-weighted weight (stream-cdr s) (stream-cdr t)))))
;; Solution:2 ends here

;; [[file:../exercise-71.org::*Solution][Solution:3]]
(define (ramanujan-weight i j)
  (+ (expt i 3) (expt j 3)))

(define (ramanujan-weight-of-list l)
  (ramanujan-weight (car l) (cadr l)))

(define (ramanujan-searcher s)
  (cond
    [(stream-null? s) the-empty-stream]
    [(= (ramanujan-weight-of-list (stream-car s))
        (ramanujan-weight-of-list (stream-car (stream-cdr s))))
     (cons-stream (ramanujan-weight-of-list (stream-car s)) (ramanujan-searcher (stream-cdr s)))]
    [else (ramanujan-searcher (stream-cdr s))]))

(define ramanujan-numbers
  (ramanujan-searcher (pairs-weighted (lambda (x) (ramanujan-weight-of-list x)) integers integers)))
;; Solution:3 ends here

;; [[file:../exercise-71.org::*Solution][Solution:4]]
(newline)
(display "first 6 ramanujans are:")
(newline)
(display-stream-range 0 5 ramanujan-numbers)
;; Solution:4 ends here
