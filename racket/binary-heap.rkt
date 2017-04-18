#lang racket/base

(require "base.rkt")

(provide binary-heap-fix-increased!
         binary-heap-fix-decreased!
         binary-heap-top
         binary-heap-push!
         binary-heap-pop-top!
         build-binary-heap!)

(define (binary-heap-fix-increased! a length lower? i)
  (let loop ([i i])
    (let ([parent (quotient (sub1 i) 2)])
      (when (and (> i 0)
                 (lower? (vector-ref a parent) (vector-ref a i)))
        (vector-swap! a parent i)
        (loop parent)))))

(define (binary-heap-fix-decreased! a length lower? i)
  (let loop ([i i])
    (let* ([left (add1 (* 2 i))]
           [right (add1 left)])
      (when (< left length)
        (let ([child (if (and (< right length)
                              (lower? (vector-ref a left) (vector-ref a right)))
                         right
                         left)])
          (when (lower? (vector-ref a i) (vector-ref a child))
            (vector-swap! a i child)
            (loop child)))))))

(define (binary-heap-top a length lower?)
  (vector-ref a 0))

(define (binary-heap-push! a length lower? x)
  (vector-set! a length x)
  (binary-heap-fix-increased! a (add1 length) lower? length))

(define (binary-heap-pop-top! a length lower?)
  (if (= length 0)
      (begin0
        (vector-ref a 0)
        (vector-set! a 0 #f))
      (begin0
        (vector-ref a 0)
        (vector-set! a 0 (vector-ref a (sub1 length)))
        (vector-set! a (sub1 length) #f)
        (binary-heap-fix-decreased! a (sub1 length) lower? 0))))

(define (build-binary-heap! a length lower?)
  (for ([i (in-range (sub1 (quotient length 2)) -1 -1)])
    (binary-heap-fix-decreased! a length lower? i)))
