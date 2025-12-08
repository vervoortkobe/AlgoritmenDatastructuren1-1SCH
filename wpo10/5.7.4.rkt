#lang r7rs

(define-library (sorting)
  (export sort)
  (import (scheme base)
          (scheme write))
  (begin

    (define (selection-sort vector <<?)
      (let outer-loop ((outer-idx 0))
        (when (< outer-idx (- (vector-length vector) 1))
          ;; find index of smallest element in [outer-idx, end)
          (let inner-loop ((inner-idx (+ outer-idx 1))
                           (smallest-idx outer-idx))
            (if (>= inner-idx (vector-length vector))
                ;; swap current position with smallest
                (let ((i outer-idx)
                      (j smallest-idx))
                  (when (not (= i j))
                    (let ((tmp (vector-ref vector i)))
                      (vector-set! vector i (vector-ref vector j))
                      (vector-set! vector j tmp))))
                (if (<<? (vector-ref vector inner-idx)
                         (vector-ref vector smallest-idx))
                    (inner-loop (+ inner-idx 1) inner-idx)
                    (inner-loop (+ inner-idx 1) smallest-idx))))
          (outer-loop (+ outer-idx 1)))
        vector))

    (define sort selection-sort)))

(define v (vector 8 9 5 4 8 6 2 8 8 9 4 54384 684 5 43 54 8))
(sort v <)
(write v)
