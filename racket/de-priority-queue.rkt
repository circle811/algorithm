#lang racket/base

(require "interval-heap.rkt")

(provide make-de-priority-queue
         de-priority-queue?
         de-priority-queue-empty?
         de-priority-queue-count
         de-priority-queue-min
         de-priority-queue-max
         de-priority-queue-push!
         de-priority-queue-pop-min!
         de-priority-queue-pop-max!)

(struct de-priority-queue (vector count <?)
  #:mutable)

(define (make-de-priority-queue <?)
  (de-priority-queue (make-vector 64) 0 <?))

(define (de-priority-queue-empty? q)
  (= (de-priority-queue-count q) 0))

(define (de-priority-queue-empty-check q)
  (when (de-priority-queue-empty? q)
    (error "de-priority-queue is empty")))

(define (de-priority-queue-min q)
  (de-priority-queue-empty-check q)
  (interval-heap-min (de-priority-queue-vector q) (de-priority-queue-count q) (de-priority-queue-<? q)))

(define (de-priority-queue-max q)
  (de-priority-queue-empty-check q)
  (interval-heap-max (de-priority-queue-vector q) (de-priority-queue-count q) (de-priority-queue-<? q)))

(define (de-priority-queue-push! q x)
  (let* ([a (de-priority-queue-vector q)]
         [length (vector-length a)])
    (when (= (de-priority-queue-count q) length)
      (let ([b (make-vector (* 2 length))])
        (vector-copy! b 0 a)
        (set-de-priority-queue-vector! q b))))
  (interval-heap-push! (de-priority-queue-vector q) (de-priority-queue-count q) (de-priority-queue-<? q) x)
  (set-de-priority-queue-count! q (add1 (de-priority-queue-count q))))

(define (de-priority-queue-pop-min! q)
  (de-priority-queue-empty-check q)
  (begin0
    (interval-heap-pop-min! (de-priority-queue-vector q) (de-priority-queue-count q) (de-priority-queue-<? q))
    (set-de-priority-queue-count! q (sub1 (de-priority-queue-count q)))))

(define (de-priority-queue-pop-max! q)
  (de-priority-queue-empty-check q)
  (begin0
    (interval-heap-pop-max! (de-priority-queue-vector q) (de-priority-queue-count q) (de-priority-queue-<? q))
    (set-de-priority-queue-count! q (sub1 (de-priority-queue-count q)))))
