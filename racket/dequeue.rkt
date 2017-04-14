#lang racket/base

(provide make-dequeue
         dequeue?
         dequeue-empty?
         dequeue-count
         dequeue-left
         dequeue-left-push!
         dequeue-left-pop!
         dequeue-right
         dequeue-right-push!
         dequeue-right-pop!)

(struct dequeue (left-block right-block low high count block-size)
  #:mutable)

(struct block (left right vector)
  #:mutable)

(define (make-dequeue [block-size 64])
  (unless (> block-size 1)
    (error "make-dequeue: block-size must greater than 1"))
  (let ([b (block #f #f (make-vector block-size))]
        [half (quotient block-size 2)])
    (dequeue b b half half 0 block-size)))

(define (dequeue-empty? q)
  (= (dequeue-count q) 0))

(define (dequeue-empty-check q)
  (when (dequeue-empty? q)
    (error "empty dequeue")))

(define (dequeue-left q)
  (dequeue-empty-check q)
  (vector-ref (block-vector (dequeue-left-block q))
              (dequeue-low q)))

(define (dequeue-left-push! q x)
  (define (push! b low)
    (vector-set! (block-vector b) (sub1 low) x)
    (set-dequeue-low! q (sub1 low)))
  (set-dequeue-count! q (add1 (dequeue-count q)))
  (let ([b (dequeue-left-block q)]
        [low (dequeue-low q)])
    (cond
      [(> low 0)
       (push! b low)]
      [(block-left b)
       => (lambda (c)
            (set-dequeue-left-block! q c)
            (push! c (dequeue-block-size q)))]
      [else
       (let ([c (block #f b (make-vector (dequeue-block-size q)))])
         (set-block-left! b c)
         (set-dequeue-left-block! q c)
         (push! c (dequeue-block-size q)))])))

(define (dequeue-left-pop! q)
  (dequeue-empty-check q)
  (set-dequeue-count! q (sub1 (dequeue-count q)))
  (let* ([b (dequeue-left-block q)]
         [low (dequeue-low q)]
         [v (block-vector b)]
         [x (vector-ref v low)])
    (vector-set! v low #f)
    (cond
      [(< (add1 low) (dequeue-block-size q))
       (set-dequeue-low! q (add1 low))]
      [(eq? b (dequeue-right-block q))
       (set-block-left! b #f)
       (let ([half (quotient (dequeue-block-size q) 2)])
         (set-dequeue-low! q half)
         (set-dequeue-high! q half))]
      [else
       (set-block-left! b #f)
       (set-dequeue-left-block! q (block-right b))
       (set-dequeue-low! q 0)])
    x))

(define (dequeue-right q)
  (dequeue-empty-check q)
  (vector-ref (block-vector (dequeue-right-block q))
              (sub1 (dequeue-high q))))

(define (dequeue-right-push! q x)
  (define (push! b high)
    (vector-set! (block-vector b) high x)
    (set-dequeue-high! q (add1 high)))
  (set-dequeue-count! q (add1 (dequeue-count q)))
  (let ([b (dequeue-right-block q)]
        [high (dequeue-high q)])
    (cond
      [(< high (dequeue-block-size q))
       (push! b high)]
      [(block-right b)
       => (lambda (c)
            (set-dequeue-right-block! q c)
            (push! c 0))]
      [else
       (let ([c (block b #f (make-vector (dequeue-block-size q)))])
         (set-block-right! b c)
         (set-dequeue-right-block! q c)
         (push! c 0))])))

(define (dequeue-right-pop! q)
  (dequeue-empty-check q)
  (set-dequeue-count! q (sub1 (dequeue-count q)))
  (let* ([b (dequeue-right-block q)]
         [high (dequeue-high q)]
         [v (block-vector b)]
         [x (vector-ref v (sub1 high))])
    (vector-set! v (sub1 high) #f)
    (cond
      [(> high 1)
       (set-dequeue-high! q (sub1 high))]
      [(eq? b (dequeue-left-block q))
       (set-block-right! b #f)
       (let ([half (quotient (dequeue-block-size q) 2)])
         (set-dequeue-low! q half)
         (set-dequeue-high! q half))]
      [else
       (set-block-right! b #f)
       (set-dequeue-right-block! q (block-left b))
       (set-dequeue-high! q (dequeue-block-size q))])
    x))
