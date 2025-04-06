#lang sicp

;; This procedure will split the donor in half, and return a pair where the
;; first element is a pointer to the donor, and the second element of the pair
;; is the pointer to the reversed tail list.
(define (split-reverse! donor)
  (define (reverse-list acc tail)
    (if (null? tail)
        acc
        (reverse-list (cons (car tail) acc) (cdr tail))))
  ;; This procedure will return the pair in the middle. If input is null, then
  ;; return null, if only one element return that element, if two element,
  ;; return first element.
  (define (middle-pair items)
    (define (iter target acc tail)
      (cond
        [(null? tail) (error "hit null" (list items tail acc))]
        [(>= acc target) tail]
        [else (iter target (+ acc 1) (cdr tail))]))
    (if (null? items)
        items
        (let ([middle-target (floor (/ (- (length items) 1) 2))]) (iter middle-target 0 items))))
  ;; This procedure changes the cdr of the pair to nil.
  (define (seal-pair! pair)
    (if (null? pair)
        (error "sealing nil" pair)
        (set-cdr! pair '())))
  (let ([target-middle (middle-pair donor)])
    (let ([rest-reversed (reverse-list '() (cdr target-middle))])
      (seal-pair! target-middle)
      (cons donor rest-reversed))))

(define (front-ptr deque)
  (car deque))
(define (rear-ptr deque)
  (cdr deque))
(define (set-front-ptr! deque item)
  (set-car! deque item))
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (make-deque)
  (cons '() '()))

(define (empty-deque? deque)
  (and (null? (car deque)) (null? (cdr deque))))

;; General head selector procedure. Returns a pair where the first element is
;; the front that is being searched for, and the second element is the result
;; from split-reverse if that procedure application was required.
(define (search-head primary-ptr secondary-ptr)
  (cond
    [(not (null? primary-ptr)) (cons (car primary-ptr) '())]
    [(null? secondary-ptr) (error "GETTER called on empty DEQUE:" (cons primary-ptr secondary-ptr))]
    [else
     (let ([rebalanced (split-reverse! secondary-ptr)])
       (if (null? (cdr rebalanced))
           (cons (car secondary-ptr) '())
           (cons (car (cdr rebalanced)) rebalanced)))]))

(define (front-deque deque)
  (let ([search-res (search-head (front-ptr deque) (rear-ptr deque))])
    (if (null? (cdr search-res))
        (car search-res)
        (let ([new-front-ptr (cdr (cdr search-res))]
              [new-rear-ptr (car (cdr search-res))])
          (begin
            (set-front-ptr! deque new-front-ptr)
            (set-rear-ptr! deque new-rear-ptr)
            (car search-res))))))

(define (rear-deque deque)
  (let ([search-res (search-head (rear-ptr deque) (front-ptr deque))])
    (if (null? (cdr search-res))
        (car search-res)
        (let ([new-front-ptr (car (cdr search-res))]
              [new-rear-ptr (cdr (cdr search-res))])
          (begin
            (set-front-ptr! deque new-front-ptr)
            (set-rear-ptr! deque new-rear-ptr)
            (car search-res))))))

(define (front-insert-deque! deque item)
  (set-front-ptr! deque (cons item (front-ptr deque)))
  deque)
(define (rear-insert-deque! deque item)
  (set-rear-ptr! deque (cons item (rear-ptr deque)))
  deque)

(define (front-delete-deque! deque)
  (let ([curr-front-ptr (front-ptr deque)])
    (cond
      [(not (null? curr-front-ptr))
       (begin
         (set-front-ptr! deque (cdr curr-front-ptr))
         deque)]
      [(null? (rear-ptr deque)) (error "DELETE on empty DEQUE" deque)]
      [else
       (let ([rebalanced (split-reverse! (rear-ptr deque))])
         (let ([new-rear (car rebalanced)]
               [new-front (cdr rebalanced)])
           (display "new-rear and new-front: ") (display (list new-rear new-front)) (newline)
           (if (null? new-front)
               (begin
                 (set-rear-ptr! deque '())
                 deque)
               (begin
                 (set-front-ptr! deque (cdr new-front))
                 (set-rear-ptr! deque new-rear)
                 deque))))])))
(define (rear-delete-deque! deque)
  (let ([curr-rear-ptr (rear-ptr deque)])
    (cond
      [(not (null? curr-rear-ptr))
       (begin
         (set-rear-ptr! deque (cdr curr-rear-ptr))
         deque)]
      [(null? (front-ptr deque)) (error "DELETE on empty DEQUE" deque)]
      [else
       (let ([rebalanced (split-reverse! (front-ptr deque))])
         (let ([new-rear (cdr rebalanced)]
               [new-front (car rebalanced)])
           (if (null? new-rear)
               (begin
                 (set-front-ptr! deque '())
                 deque)
               (begin
                 (set-front-ptr! deque new-front)
                 (set-rear-ptr! deque (cdr new-rear))
                 deque))))])))

(define deque1 (make-deque))
(empty-deque? deque1)
(front-insert-deque! deque1 'd)
(front-insert-deque! deque1 'c)
(front-insert-deque! deque1 'b)
(front-insert-deque! deque1 'a)
(define deque2 (make-deque))
(rear-insert-deque! deque2 'd)
(rear-insert-deque! deque2 'c)
(rear-insert-deque! deque2 'b)
(rear-insert-deque! deque2 'a)

(define (assert-eq msg left right)
  (display (if (eq? left right) "pass: " "fail: "))
  (display msg)
  (newline))

(assert-eq "front of (a b c d)" (front-deque deque1) 'a)
(assert-eq "front of (d c b a)" (front-deque deque2) 'd)
(assert-eq "rear of (a b c d)" (rear-deque deque1) 'd)
(assert-eq "rear of (d c b a)" (rear-deque deque2) 'a)

(front-delete-deque! deque1)
(assert-eq "front of (b c d)" (front-deque deque1) 'b)
(front-delete-deque! deque1)
(assert-eq "front of (c d)" (front-deque deque1) 'c)
(front-delete-deque! deque1)
(assert-eq "front of (d)" (front-deque deque1) 'd)
(front-delete-deque! deque1)
(assert-eq "should be empty deque after this line" (empty-deque? deque1) true)

(assert-eq "Testing (nil (a))"
           (empty-deque? (front-delete-deque! (rear-insert-deque! (make-deque) 'a)))
           true)

(rear-delete-deque! deque2)
(assert-eq "rear of (d c b)" (rear-deque deque2) 'b)
(rear-delete-deque! deque2)
(assert-eq "rear of (d c)" (rear-deque deque2) 'c)
(rear-delete-deque! deque2)
(assert-eq "rear of (d)" (rear-deque deque2) 'd)
(rear-delete-deque! deque2)
(assert-eq "should be empty deque after this line" (empty-deque? deque2) true)
