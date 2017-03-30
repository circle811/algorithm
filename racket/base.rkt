#lang racket/base

(provide random-range
         vector-swap!)

(define (random-range start end)
  (+ start (random (- end start))))

(define (vector-swap! a i j)
  (let ([x (vector-ref a i)])
    (vector-set! a i (vector-ref a j))
    (vector-set! a j x)))
