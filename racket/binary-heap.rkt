#lang racket/base

(require "base.rkt")

(provide build-heap!
         heap-insert!
         heap-extract-top!
         heap-move-up!
         heap-move-down!)

(define (build-heap! a length lower?)
  (for ([i (in-range (sub1 (quotient length 2)) -1 -1)])
    (heap-move-down! a length lower? i)))

(define (heap-insert! a length lower? x)
  (vector-set! a length x)
  (heap-move-up! a (add1 length) lower? length))

(define (heap-extract-top! a length lower?)
  (let ([top (vector-ref a 0)])
    (vector-set! a 0 (vector-ref a (sub1 length)))
    (vector-set! a (sub1 length) #f)
    (heap-move-down! a (sub1 length) lower? 0)
    top))

(define (heap-move-up! a length lower? i)
  (let loop ([i i])
    (let ([p (quotient (sub1 i) 2)])
      (when (and (> i 0)
                 (lower? (vector-ref a p) (vector-ref a i)))
        (vector-swap! a p i)
        (loop p)))))

(define (heap-move-down! a length lower? i)
  (let loop ([i i])
    (let* ([l (add1 (* 2 i))]
           [r (add1 l)])
      (when (< l length)
        (let ([c (if (and (< r length)
                          (lower? (vector-ref a l) (vector-ref a r)))
                     r
                     l)])
          (when (lower? (vector-ref a i) (vector-ref a c))
            (vector-swap! a i c)
            (loop c)))))))
