;; [[file:../../stream-base.org::*Module base][Module base:1]]
#lang sicp
;; Module base:1 ends here

;; [[file:../../stream-base.org::*Module base][Module base:2]]
(#%provide stream-car
           stream-cdr
           stream-for-each
           display-stream
           stream-ref
           display-line
           stream-enumerate-interval)
;; Module base:2 ends here

;; [[file:../../stream-base.org::*Stream basic selectors][Stream basic selectors:1]]
(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))
;; Stream basic selectors:1 ends here

;; [[file:../../stream-base.org::*Stream Utilities][Stream Utilities:1]]
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))
;; procedure that displays all the elements of a stream
(define (display-stream s)
  (display "(")
  (stream-for-each display-ele s)
  (display ")"))
(define (display-line x)
  (newline)
  (display x))
(define (display-ele x)
  (display " ")
  (display x))
;; Stream Utilities:1 ends here

;; [[file:../../stream-base.org::*Stream Utilities][Stream Utilities:2]]
;; procedure that does a 0-index based lookup of the stream
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
;; Stream Utilities:2 ends here

;; [[file:../../stream-base.org::*Stream Utilities][Stream Utilities:3]]
;; procedure that will generate a stream of a range of value, with the range
;; being [start, end]
(define (stream-enumerate-interval start end)
  (if (> start end)
      the-empty-stream
      (cons-stream
       start
       (stream-enumerate-interval (+ start 1) end))))
;; Stream Utilities:3 ends here
