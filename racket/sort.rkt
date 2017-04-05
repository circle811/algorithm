#lang racket/base

(require "base.rkt"
         "binary-heap.rkt")

(provide list-insertion-sort
         list-selection-sort
         list-merge-sort
         list-quick-sort
         vector-insertion-sort!
         vector-binary-insertion-sort!
         vector-selection-sort!
         vector-merge-sort!
         vector-quick-sort!
         vector-heap-sort!
         vector-radix-sort
         vector-bucket-sort)

(define (list-insertion-sort a <?)
  (define (insert x a)
    (cond
      [(null? a)
       (list x)]
      [(<? (car a) x)
       (cons (car a)
             (insert x (cdr a)))]
      [else
       (cons x a)]))
  (define (sort a)
    (if (null? a)
        '()
        (insert (car a)
                (sort (cdr a)))))
  (sort a))

(define (list-selection-sort a <?)
  (define (select a)
    (if (null? (cdr a))
        (values (car a) '())
        (let-values ([(min others) (select (cdr a))])
          (if (<? min (car a))
              (values min (cons (car a) others))
              (values (car a) (cdr a))))))
  (define (sort a)
    (if (null? a)
        '()
        (let-values ([(min others) (select a)])
          (cons min (sort others)))))
  (sort a))

(define (list-merge-sort a <?)
  (define (divide a)
    (let loop ([a a]
               [b '()]
               [c a])
      (if (or (null? a) (null? (cdr a)))
          (values (reverse b) c)
          (loop (cddr a)
                (cons (car c) b)
                (cdr c)))))
  (define (merge b c)
    (cond
      [(null? b)
       c]
      [(null? c)
       b]
      [(<? (car c) (car b))
       (cons (car c)
             (merge b (cdr c)))]
      [else
       (cons (car b)
             (merge (cdr b) c))]))
  (define (sort a)
    (if (or (null? a) (null? (cdr a)))
        a
        (let-values ([(b c) (divide a)])
          (merge (sort b) (sort c)))))
  (sort a))

(define (list-quick-sort a <?)
  (define (partition x a)
    (let loop ([a a]
               [b '()]
               [c '()])
      (cond
        [(null? a)
         (values (reverse b) (reverse c))]
        [(<? (car a) x)
         (loop (cdr a) (cons (car a) b) c)]
        [else
         (loop (cdr a) b (cons (car a) c))])))
  (define (sort a)
    (if (or (null? a) (null? (cdr a)))
        a
        (let-values ([(b c) (partition (car a) (cdr a))])
          (append (sort b)
                  (list (car a))
                  (sort c)))))
  (sort a))

(define (vector-back-search a low high pred)
  (let loop ([high high])
    (cond
      [(= low high)
       low]
      [(pred (vector-ref a (sub1 high)))
       high]
      [else
       (loop (sub1 high))])))

(define (vector-binary-search a low high pred)
  (let loop ([low low]
             [high high])
    (if (= low high)
        low
        (let ([mid (quotient (+ low high) 2)])
          (if (pred (vector-ref a mid))
              (loop (add1 mid) high)
              (loop low mid))))))

(define (vector-partial-insertion-sort-s! a low high <? search)
  (for ([i (in-range (add1 low) high)])
    (let* ([x (vector-ref a i)]
           [j (search a low i
                      (lambda (y)
                        (not (<? x y))))])
      (vector-copy! a (add1 j) a j i)
      (vector-set! a j x))))

(define (vector-partial-insertion-sort! a low high <?)
  (vector-partial-insertion-sort-s! a low high <? vector-back-search))

(define (vector-insertion-sort! a <?)
  (vector-partial-insertion-sort-s! a 0 (vector-length a) <? vector-back-search))

(define (vector-binary-insertion-sort! a <?)
  (vector-partial-insertion-sort-s! a 0 (vector-length a) <? vector-binary-search))

(define (vector-selection-sort! a <?)
  (define (select a low high)
    (for/fold ([pos low]
               [min (vector-ref a low)])
              ([j (in-range (add1 low) high)])
      (let ([x (vector-ref a j)])
        (if (<? x min)
            (values j x)
            (values pos min)))))
  (let ([length (vector-length a)])
    (for ([i (in-range 0 (sub1 length))])
      (let-values ([(pos min) (select a i length)])
        (vector-copy! a (add1 i) a i pos)
        (vector-set! a i min)))))

(define N 16)

(define (vector-merge-sort! a <?)
  (define (merge! a a-low b b-low b-high c c-low c-high)
    (let loop ([i a-low]
               [j b-low]
               [k c-low])
      (cond
        [(= j b-high)
         (vector-copy! a i c k c-high)]
        [(= k c-high)
         (vector-copy! a i b j b-high)]
        [(<? (vector-ref c k) (vector-ref b j))
         (vector-set! a i (vector-ref c k))
         (loop (add1 i) j (add1 k))]
        [else
         (vector-set! a i (vector-ref b j))
         (loop (add1 i) (add1 j) k)])))
  (define (sort! b b-low c c-low c-high)
    (let* ([length (- c-high c-low)]
           [b-high (+ b-low length)])
      (if (and (<= length N)
               (eq? b a))
          (vector-partial-insertion-sort! b b-low b-high <?)
          (let* ([half (quotient (add1 length) 2)]
                 [b-mid (+ b-low half)]
                 [c-mid (+ c-low half)])
            (sort! c c-low b b-low b-mid)
            (sort! c c-mid b b-mid b-high)
            (merge! b b-low c c-low c-mid c c-mid c-high)))))
  (let ([length (vector-length a)])
    (if (<= length N)
        (vector-partial-insertion-sort! a 0 length <?)
        (let* ([half (quotient (add1 length) 2)]
               [b (make-vector half)])
          (sort! a half b 0 (- length half))
          (sort! b 0 a 0 half)
          (merge! a 0 b 0 half a half length)))))

(define (vector-quick-sort! a <?)
  (define (partition! x low high)
    (for/fold ([sep low])
              ([j (in-range low high)])
      (if (if (even? j)
              (<? (vector-ref a j) x)
              (not (<? x (vector-ref a j))))
          (begin
            (vector-swap! a sep j)
            (add1 sep))
          sep)))
  (define (sort! low high)
    (if (<= (- high low) N)
        (vector-partial-insertion-sort! a low high <?)
        (begin
          (vector-swap! a low (random-range low high))
          (let ([sep (partition! (vector-ref a low) (add1 low) high)])
            (vector-swap! a low (sub1 sep))
            (sort! low (sub1 sep))
            (sort! sep high)))))
  (sort! 0 (vector-length a)))

(define (vector-heap-sort! a <?)
  (let ([length (vector-length a)])
    (build-heap! a length <?)
    (for ([i (in-range (sub1 length) 0 -1)])
      (vector-swap! a 0 i)
      (heap-move-down! a i <? 0))))

(define (vector-counting-sort a key key-count)
  (let ([b (make-vector (vector-length a))]
        [c (make-vector key-count 0)])
    (for ([x a])
      (let ([k (key x)])
        (vector-set! c k (add1 (vector-ref c k)))))
    (for/fold ([start 0])
              ([i (in-range 0 key-count)])
      (begin0
        (+ start (vector-ref c i))
        (vector-set! c i start)))
    (for ([i (in-naturals)]
          [x a])
      (let* ([k (key x)]
             [pos (vector-ref c k)])
        (vector-set! b pos x)
        (vector-set! c k (add1 pos))))
    b))

(define (vector-radix-sort a key)
  (let* ([key-max (for/fold ([m 0])
                            ([x a])
                    (max m (key x)))]
         [total-bit-length (integer-length key-max)]
         [pass-bit-length (max (integer-length (vector-length a)) 4)]
         [key-count (arithmetic-shift 1 pass-bit-length)]
         [key-mask (sub1 key-count)])
    (for/fold ([b a])
              ([bit-start (in-range 0 total-bit-length pass-bit-length)])
      (vector-counting-sort b
                            (lambda (x)
                              (bitwise-and (arithmetic-shift (key x)
                                                             (- bit-start))
                                           key-mask))
                            key-count))))

(define (vector-bucket-sort a key)
  (let* ([length (vector-length a)]
         [b (vector-counting-sort a
                                  (lambda (x)
                                    (inexact->exact
                                     (floor (* length (key x)))))
                                  length)])
    (vector-insertion-sort! b
                            (lambda (x y)
                              (< (key x) (key y))))
    b))
