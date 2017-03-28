#lang racket/base

(provide list-insertion-sort
         list-selection-sort
         list-merge-sort
         list-quick-sort)

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
