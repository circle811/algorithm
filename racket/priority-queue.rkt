#lang racket/base

(require "binary-heap.rkt")

(provide make-priority-queue
         priority-queue?
         priority-queue-empty?
         priority-queue-count
         priority-queue-top
         priority-queue-push!
         priority-queue-pop-top!)

(struct priority-queue (vector count lower?)
  #:mutable)

(define (make-priority-queue lower?)
  (priority-queue (make-vector 64) 0 lower?))

(define (priority-queue-empty? q)
  (= (priority-queue-count q) 0))

(define (priority-queue-empty-check q)
  (when (priority-queue-empty? q)
    (error "priority-queue is empty")))

(define (priority-queue-top q)
  (priority-queue-empty-check q)
  (binary-heap-top (priority-queue-vector q) (priority-queue-count q) (priority-queue-lower? q)))

(define (priority-queue-push! q x)
  (let* ([a (priority-queue-vector q)]
         [length (vector-length a)])
    (when (= (priority-queue-count q) length)
      (let ([b (make-vector (* 2 length))])
        (vector-copy! b 0 a)
        (set-priority-queue-vector! q b))))
  (binary-heap-push! (priority-queue-vector q) (priority-queue-count q) (priority-queue-lower? q) x)
  (set-priority-queue-count! q (add1 (priority-queue-count q))))

(define (priority-queue-pop-top! q)
  (priority-queue-empty-check q)
  (begin0
    (binary-heap-pop-top! (priority-queue-vector q) (priority-queue-count q) (priority-queue-lower? q))
    (set-priority-queue-count! q (sub1 (priority-queue-count q)))))
