#lang sicp

(define (make-queue)
  (let ([front-ptr '()]
        [rear-ptr '()])
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" front-ptr)
          (car front-ptr)))
    (define (insert-queue! item)
      (let ([new-pair (cons item '())])
        (cond
          [(empty-queue?)
           (set-front-ptr! new-pair)
           (set-rear-ptr! new-pair)]
          [else
           (set-cdr! rear-ptr new-pair)
           (set-rear-ptr! new-pair)])))
    (define (delete-queue!)
      (cond
        [(empty-queue?) (error "DELETE! called with an empty queue")]
        [else (set-front-ptr! (cdr front-ptr))]))
    (define (dispatch m)
      (cond
        [(eq? m 'front-ptr) front-ptr]
        [(eq? m 'rear-ptr) rear-ptr]
        [(eq? m 'empty-queue?) (empty-queue?)]
        [(eq? m 'front-queue) (front-queue)]
        [(eq? m 'insert-queue!) insert-queue!]
        [(eq? m 'delete-queue!) (delete-queue!)]
        [else (error "DISPATCH called with invalid symbol" m)]))
    dispatch))
(define (front-ptr queue)
  (queue 'front-ptr))
(define (rear-ptr queue)
  (queue 'rear-ptr))
(define (empty-queue? queue)
  (queue 'empty-queue?))
(define (front-queue queue)
  (queue 'front-queue))
(define (insert-queue! queue item)
  ((queue 'insert-queue!) item)
  queue)
(define (delete-queue! queue)
  (queue 'delete-queue!)
  queue)
(define (print-queue queue)
  (display (front-ptr queue))
  (newline))

(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))
