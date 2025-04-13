#lang sicp

(define (make-table same-key? less-than-key?)
  ;; Binary tree representation, internal to the table instance
  (define (make-tree entry left right)
    (list entry left right))
  (define (entry tree)
    (car tree))
  (define (left-branch tree)
    (cadr tree))
  (define (right-branch tree)
    (caddr tree))
  (define (set-left-branch! value tree)
    (set-car! value (cdr tree)))
  (define (set-right-branch! value tree)
    (set-car! value (cddr tree)))

  ;; For the sake of future abstraction, we should separate out the
  ;; interactions with the tables and records
  (define (insert-record! table-head record)
    (let ([rec-key (car record)]
          [rec-val (cdr record)])
      (define (insert-record-aux! table-node)
        (let ([curr-node-record (entry table-node)])
          (cond
            [(same-key? (car curr-node-record) rec-key) (set-cdr! curr-node-record rec-val)]
            [(less-than-key? (car (curr-node-record)) rec-key)
             (let ([target-branch (left-branch table-node)])
               (if (null? target-branch)
                   (set-left-branch! (make-tree record '() '()) table-node)
                   (insert-record-aux! target-branch)))]
            [else
             (let ([target-branch (right-branch table-node)])
               (if (null? target-branch)
                   (set-right-branch! (make-tree record '() '()) table-node)
                   (insert-record-aux! target-branch)))])))
      (if (null? (cdr table-head))
          (set-cdr! table-head (make-tree record '() '()))
          (insert-record-aux! (cdr table-head)))))

  ;; Main addition to this exercise is the following closure. We are now
  ;; creating custom assoc procedure for the different table instances.
  ;; The records parameter is now the current root node of the table
  (define (assoc key records)
    (cond
      [(null? records) false]
      [(same-key? key (car (entry records))) (entry records)]
      [(less-than-key? key (car (entry records))) (assoc key (left-branch records))]
      [else (assoc key (right-branch records))]))
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
                        (insert-record! record new-record)
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

(define number-table (make-table = <))

(define (assert-eq? msg value expected)
  (display (if (eq? value expected) "pass: " "fail: "))
  (display msg)
  (display " ")
  (display "expected: ")
  (display expected)
  (display " got: ")
  (display value)
  (newline))

(insert! (list 42.0 138/2) 'b number-table)
(assert-eq? "exact value lookup for number table" (lookup (list 42.0 138/2) number-table) 'b)
(assert-eq? "equivalent lookup for number table" (lookup (list 42 69) number-table) 'b)
