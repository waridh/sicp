;; [[file:../../stream-base.org::*Module base][Module base:1]]
#lang sicp
;; Module base:1 ends here

;; [[file:../../stream-base.org::*Module base][Module base:2]]
(#%provide stream-car stream-cdr display-stream)
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
