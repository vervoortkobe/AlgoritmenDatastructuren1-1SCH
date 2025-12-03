#lang r7rs
(import (scheme base)
        (scheme write))

(define (bubble-sort vector <<?)
  (define (bubble-swap vector idx1 idx2)
    (let ((keep (vector-ref vector idx1)))
      (vector-set! vector idx1 (vector-ref vector idx2))
      (vector-set! vector idx2 keep)
      #t))
  (let outer-loop
    ((unsorted-idx (- (vector-length vector) 2)))
    (if (>= unsorted-idx 0)
        (if (let inner-loop
              ((inner-idx 0)
               (has-changed? #f))
              (if (> inner-idx unsorted-idx)
                  has-changed?
                  (inner-loop (+ inner-idx 1)
                             (if (<<? (vector-ref vector (+ inner-idx 1))
                                      (vector-ref vector inner-idx))
                                 (bubble-swap vector inner-idx (+ inner-idx 1))
                                 has-changed?))))
            (outer-loop (- unsorted-idx 1))))))

(define (bubble-sort-list lst <<?)
  (define (bubble-swap-list current)
    (let ((keep (car current)))
      (set-car! current (cadr current))
      (set-car! (cdr current) keep)
      #t))
  (let outer-loop
    ((unsorted-idx (- (length lst) 2)))
    (if (>= unsorted-idx 0)
        (if (let inner-loop
              ((inner-idx 0)
               (has-changed? #f)
               (current lst))
              (if (> inner-idx unsorted-idx)
                  has-changed?
                  (inner-loop (+ inner-idx 1)
                             (if (<<? (cadr current) (car current))
                                 (bubble-swap-list current)
                                 has-changed?)
                             (cdr current))))
            (outer-loop (- unsorted-idx 1))))))

(define l '(5 87 9 2 0 489 487684984654 0 948 2))
(bubble-sort-list l <)
(display l)
