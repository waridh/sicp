#lang sicp

;;; This module contains the different procedures that facilitates data-driven design.

(#%provide entry? entry-key entry-value make-table add-entry get-entry get-value)

(define (entry? e)
  (and (pair? e) (not (null? (cdr e)))))

(define (entry-key e)
  "selector for the key to the entries that are stored in the assoc list"
  (if (null? e)
      false
      (car e)))

(define (entry-value e)
  "selector for the value of the entries that are used in the assoc list"
  (if (null? e)
      false
      (cdr e)))

(define (make-table)
  (let ([table '()])
    (lambda (s)
      (cond
        [(eq? s 'add-entry) (lambda (entry) (set! table (cons entry table)))]
        [(eq? s 'get-entry) (lambda (key) (assoc key table))]
        [(not (symbol? s)) (error "MAKE-TABLE: Expects symbol, but did not receive symbol" s)]
        [else (error "MAKE-TABLE: Found undefined symbol:" s)]))))

(define (add-entry t k v)
  ((t 'add-entry) (cons k v)))

(define (get-entry t k)
  "procedure that returns the entry"
  ((t 'get-entry) k))

(define (get-value t k)
  "procedure that will return the value associated with the provided key in
   the table, else returns false"
  (let ([retrieved (get-entry t k)])
    (if (eq? retrieved false)
        false
        (entry-value retrieved))))
