#lang racket/base

(require "base.rkt")

(provide interval-heap-fix-increased!
         interval-heap-fix-decreased!
         interval-heap-min
         interval-heap-max
         interval-heap-push!
         interval-heap-pop-min!
         interval-heap-pop-max!)

(define (parent-min-index i)
  (* 2 (quotient (- i 2) 4)))

(define (left-min-index i)
  (+ 2 (* 4 (quotient i 2))))

(define (interval-heap-fix-increased! a length <? i)
  (define (check i j)
    (when (<? (vector-ref a j) (vector-ref a i))
      (vector-swap! a i j)
      (fix j)))
  (define (fix i)
    (if (even? i)
        (let* ([left-min (left-min-index i)]
               [right-min (+ 2 left-min)]
               [self-max (add1 i)])
          (cond
            [(< right-min length)
             (check i
                    (if (<? (vector-ref a right-min) (vector-ref a left-min))
                        right-min
                        left-min))]
            [(< left-min length)
             (check i left-min)]
            [(< self-max length)
             (check i self-max)]
            [(> i 1)
             (check i (add1 (parent-min-index i)))]))
        (when (> i 1)
          (check i (add1 (parent-min-index i))))))
  (fix i))

(define (interval-heap-fix-decreased! a length <? i)
  (define (check i j)
    (when (<? (vector-ref a i) (vector-ref a j))
      (vector-swap! a i j)
      (fix j)))
  (define (fix i)
    (if (even? i)
        (when (> i 1)
          (check i (parent-min-index i)))
        (let* ([left-min (left-min-index i)]
               [left-max (add1 left-min)]
               [right-min (+ 2 left-min)]
               [right-max (+ 3 left-min)]
               [self-min (sub1 i)])
          (cond
            [(< right-min length)
             (let ([real-right-max (if (< right-max length)
                                       right-max
                                       right-min)])
               (check i
                      (if (<? (vector-ref a left-max) (vector-ref a real-right-max))
                          real-right-max
                          left-max)))]
            [(< left-min length)
             (let ([real-left-max (if (< left-max length)
                                      left-max
                                      left-min)])
               (check i real-left-max))]
            [else
             (check i self-min)]))))
  (fix i))

(define (interval-heap-min a length <?)
  (vector-ref a 0))

(define (interval-heap-max a length <?)
  (vector-ref a (if (= length 1) 0 1)))

(define (interval-heap-push! a length <? x)
  (vector-set! a length x)
  (interval-heap-fix-decreased! a (add1 length) <? length)
  (interval-heap-fix-increased! a (add1 length) <? length))

(define (interval-heap-pop-min! a length <?)
  (if (= length 1)
      (begin0
        (vector-ref a 0)
        (vector-set! a 0 #f))
      (begin0
        (vector-ref a 0)
        (vector-set! a 0 (vector-ref a (sub1 length)))
        (vector-set! a (sub1 length) #f)
        (interval-heap-fix-increased! a (sub1 length) <? 0))))

(define (interval-heap-pop-max! a length <?)
  (if (<= length 2)
      (begin0
        (vector-ref a (sub1 length))
        (vector-set! a (sub1 length) #f))
      (begin0
        (vector-ref a 1)
        (vector-set! a 1 (vector-ref a (sub1 length)))
        (vector-set! a (sub1 length) #f)
        (interval-heap-fix-decreased! a (sub1 length) <? 1))))
