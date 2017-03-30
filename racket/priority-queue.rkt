#lang racket/base

(require "binary-heap.rkt")

(provide make-priority-queue
         priority-queue?
         priority-queue-empty?
         priority-queue-length
         priority-queue-top
         priority-queue-push!
         priority-queue-pop-top!)

(struct priority-queue (vector length lower?)
  #:mutable)

(define N 64)

(define (make-priority-queue lower?)
  (priority-queue (make-vector N) 0 lower?))

(define (priority-queue-empty? q)
  (= (priority-queue-length q) 0))

(define (priority-queue-empty-check q)
  (when (priority-queue-empty? q)
    (error "empty priority queue")))

(define (priority-queue-top q)
  (priority-queue-empty-check q)
  (vector-ref (priority-queue-vector q) 0))

(define (priority-queue-push! q x)
  (let* ([a (priority-queue-vector q)]
         [max (vector-length a)])
    (when (= (priority-queue-length q) max)
      (let ([b (make-vector (* 2 max))])
        (vector-copy! b 0 a)
        (set-priority-queue-vector! q b))))
  (heap-insert! (priority-queue-vector q) (priority-queue-length q) (priority-queue-lower? q) x)
  (set-priority-queue-length! q (add1 (priority-queue-length q))))

(define (priority-queue-pop-top! q)
  (priority-queue-empty-check q)
  (begin0
    (heap-extract-top! (priority-queue-vector q) (priority-queue-length q) (priority-queue-lower? q))
    (set-priority-queue-length! q (sub1 (priority-queue-length q)))))
