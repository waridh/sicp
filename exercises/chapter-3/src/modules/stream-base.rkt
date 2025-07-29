;; [[file:../../stream-base.org::*Module base][Module base:1]]
#lang sicp
;; Module base:1 ends here

;; [[file:../../stream-base.org::*Module base][Module base:2]]
(#%provide stream-car
           stream-cdr
           stream-for-each
           display-stream
           display-stream-range
           stream-ref
           display-line
           stream-enumerate-interval
           stream-filter
           stream-map
           make-tableau
           accelerated-sequence)
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
(define (display-stream-range lower upper s)
  (define (display-range-iter idx s-aux)
    (cond
      [(stream-null? s-aux) 'done]
      [(and (>= idx lower) (<= idx upper))
       (begin
         (display-ele (stream-car s-aux))
         (display-range-iter (+ idx 1) (stream-cdr s-aux)))]
      [(> idx upper) 'done]
      [else (display-range-iter (+ idx 1) (stream-cdr s-aux))]))
  (display "(")
  (display-range-iter 0 s)
  (display ")"))
;; Stream Utilities:2 ends here

;; [[file:../../stream-base.org::*Stream Utilities][Stream Utilities:3]]
;; procedure that does a 0-index based lookup of the stream
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; procedure that filters the stream sequence to only elements that matches the
;; provided predicate.
(define (stream-filter predicate s)
  (cond
    [(stream-null? s) the-empty-stream]
    [(predicate (stream-car s))
     (cons-stream (stream-car s) (stream-filter predicate (stream-cdr s)))]
    [else (stream-filter predicate (stream-cdr s))]))
;; Stream Utilities:3 ends here

;; [[file:../../stream-base.org::*Stream Utilities][Stream Utilities:4]]
;; procedure that will generate a stream of a range of value, with the range
;; being [start, end]
(define (stream-enumerate-interval start end)
  (if (> start end)
      the-empty-stream
      (cons-stream
       start
       (stream-enumerate-interval (+ start 1) end))))
;; Stream Utilities:4 ends here

;; [[file:../../stream-base.org::*Stream Map][Stream Map:1]]
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map (cons proc (map stream-cdr argstreams))))))
;; Stream Map:1 ends here

;; [[file:../../stream-base.org::*Make Tableau][Make Tableau:1]]
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
;; Make Tableau:1 ends here

;; [[file:../../stream-base.org::*Accelerated Sequence][Accelerated Sequence:1]]
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
;; Accelerated Sequence:1 ends here
