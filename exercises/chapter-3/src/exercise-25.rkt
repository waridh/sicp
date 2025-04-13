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
    (define (lookup keys)
      (define (iter record keys)
        (if (null? keys)
            (cdr record)
            (let ([head-key (car keys)]
                  [tail-keys (cdr keys)])
              (let ([next-record (assoc head-key (cdr record))])
                (if (not next-record)
                    false
                    (iter next-record tail-keys))))))
      (iter local-table keys))
    (define (insert! keys value)
      (define (iter! record keys value)
        (if (null? keys)
            (set-cdr! record value)
            (let ([head-key (car keys)]
                  [tail-key (cdr keys)]
                  [record-backbone (cdr record)])
              (let ([next-record (assoc head-key record-backbone)])
                (if (not next-record)
                    ;; Need to create the next nested table
                    (let ([new-record (list head-key)])
                      (begin
                        (set-cdr! record (cons new-record record-backbone))
                        (iter! new-record tail-key value)))
                    (iter! next-record tail-key value))))))
      (iter! local-table keys value)
      'ok)
    (define (dispatch m)
      (cond
        [(eq? m 'lookup-proc) lookup]
        [(eq? m 'insert-proc!) insert!]
        [else (error "Unknown operation: TABLE" m)]))
    dispatch))

(define (lookup keys table)
  ((table 'lookup-proc) keys))
(define (insert! keys value table)
  ((table 'insert-proc!) keys value))

(define basic-table (make-table equal?))
(define number-table (make-table =))

(define (assert-eq? msg value expected)
  (display (if (eq? value expected) "pass: " "fail: "))
  (display msg)
  (newline))

(insert! (list 'key-a 'key-b) 'a basic-table)
(assert-eq? "testing original table insertion and lookup"
            (lookup (list 'key-a 'key-b) basic-table)
            'a)
(insert! (list 42.0 138/2) 'b number-table)
(assert-eq? "exact value lookup for number table" (lookup (list 42.0 138/2) number-table) 'b)
(assert-eq? "equivalent lookup for number table" (lookup (list 42 69) number-table) 'b)
(insert! (list 'key-1 'key-2 'key-3) 'x basic-table)
(assert-eq? "arbitrary key insertion and lookup testing"
            (lookup (list 'key-1 'key-2 'key-3) basic-table)
            'x)
