#lang racket/base

(provide make-dequeue
         dequeue?
         dequeue-empty?
         dequeue-length
         dequeue-left-element
         dequeue-left-push!
         dequeue-left-pop!
         dequeue-right-element
         dequeue-right-push!
         dequeue-right-pop!)

(struct dequeue (left right low high length)
  #:mutable)

(struct block (left right vector)
  #:mutable)

(define N 64)
(define HALF (quotient N 2))

(define (make-dequeue)
  (let ([b (block #f #f (make-vector N))])
    (dequeue b b HALF HALF 0)))

(define (dequeue-empty? q)
  (= (dequeue-length q) 0))

(define (dequeue-empty-check q)
  (when (dequeue-empty? q)
    (error "empty dequeue")))

(define (dequeue-left-element q)
  (dequeue-empty-check q)
  (vector-ref (block-vector (dequeue-left q))
              (dequeue-low q)))

(define (dequeue-left-push! q x)
  (set-dequeue-length! q (add1 (dequeue-length q)))
  (let ([b (dequeue-left q)]
        [low (dequeue-low q)])
    (if (> low 0)
        (begin
          (vector-set! (block-vector b) (sub1 low) x)
          (set-dequeue-low! q (sub1 low)))
        (let ([c (block #f b (make-vector N))])
          (vector-set! (block-vector c) (sub1 N) x)
          (set-dequeue-low! q (sub1 N))
          (set-block-left! b c)
          (set-dequeue-left! q c)))))

(define (dequeue-left-pop! q)
  (dequeue-empty-check q)
  (set-dequeue-length! q (sub1 (dequeue-length q)))
  (let* ([b (dequeue-left q)]
         [low (dequeue-low q)]
         [v (block-vector b)]
         [x (vector-ref v low)])
    (vector-set! v low #f)
    (set-dequeue-low! q (add1 low))
    (when (= (dequeue-low q) N)
      (let ([c (block-right b)])
        (if c
            (begin
              (set-dequeue-low! q 0)
              (set-block-left! c #f)
              (set-dequeue-left! q c))
            (begin
              (set-dequeue-low! q HALF)
              (set-dequeue-high! q HALF)))))
    x))

(define (dequeue-right-element q)
  (dequeue-empty-check q)
  (vector-ref (block-vector (dequeue-right q))
              (sub1 (dequeue-high q))))

(define (dequeue-right-push! q x)
  (set-dequeue-length! q (add1 (dequeue-length q)))
  (let ([b (dequeue-right q)]
        [high (dequeue-high q)])
    (if (< high N)
        (begin
          (vector-set! (block-vector b) high x)
          (set-dequeue-high! q (add1 high)))
        (let ([c (block b #f (make-vector N))])
          (vector-set! (block-vector c) 0 x)
          (set-dequeue-high! q 1)
          (set-block-right! b c)
          (set-dequeue-right! q c)))))

(define (dequeue-right-pop! q)
  (dequeue-empty-check q)
  (set-dequeue-length! q (sub1 (dequeue-length q)))
  (let* ([b (dequeue-right q)]
         [high (dequeue-high q)]
         [v (block-vector b)]
         [x (vector-ref v (sub1 high))])
    (vector-set! v (sub1 high) #f)
    (set-dequeue-high! q (sub1 high))
    (when (= (dequeue-high q) 0)
      (let ([c (block-left b)])
        (if c
            (begin
              (set-dequeue-high! q N)
              (set-block-right! c #f)
              (set-dequeue-right! q c))
            (begin
              (set-dequeue-low! q HALF)
              (set-dequeue-high! q HALF)))))
    x))
