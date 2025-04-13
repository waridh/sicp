#lang sicp

(define (make-table same-key?)
  ;; Main addition to this exercise is the following closure. We are now
  ;; creating custom assoc procedure for the different table instances.
  (define (assoc key records)
    (cond
      [(null? records) false]
      [(same-key? key (caar records)) (car records)]
      [else (assoc key (cdr records))]))
  (let ([local-table (list '*table*)])
    (define (lookup key-1 key-2)
      (let ([subtable (assoc key-1 (cdr local-table))])
        (if subtable
            (let ([record (assoc key-2 (cdr subtable))])
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ([subtable (assoc key-1 (cdr local-table))])
        (if subtable
            (let ([record (assoc key-2 (cdr subtable))])
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table (cons (list key-1 (cons key-2 value)) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        [(eq? m 'lookup-proc) lookup]
        [(eq? m 'insert-proc!) insert!]
        [else (error "Unknown operation: TABLE" m)]))
    dispatch))

(define (lookup key-1 key-2 table )
  ((table 'lookup-proc) key-1 key-2))
(define (insert! key-1 key-2 value table)
  ((table 'insert-proc!) key-1 key-2 value))

(define basic-table (make-table equal?))
(define number-table (make-table =))

(define (assert-eq? msg value expected)
  (display (if (eq? value expected) "pass: " "fail: "))
  (display msg)
  (newline))

(insert! 'key-a 'key-b 'a basic-table)
(assert-eq? "testing original table insertion and lookup" (lookup 'key-a 'key-b basic-table) 'a)
(insert! 42.0 138/2 'b number-table)
(assert-eq? "exact value lookup for number table" (lookup 42.0 138/2 number-table) 'b)
(assert-eq? "equivalent lookup for number table" (lookup 42 69 number-table) 'b)
