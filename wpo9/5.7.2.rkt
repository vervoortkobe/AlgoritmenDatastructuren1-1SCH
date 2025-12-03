#lang r7rs

(import (scheme base)
        (scheme write))

(define (insertion-sort vector <<?)
  (define (>=? x y) (not (<<? x y)))
  (let outer-loop
    ((outer-idx (- (vector-length vector) 2)))
      (let 
        ((current (vector-ref vector outer-idx)))
        (vector-set!
          vector
          (let inner-loop
            ((inner-idx (+ 1 outer-idx)))
            (cond
              ((or (>= inner-idx (vector-length vector))
                   (>=? (vector-ref vector inner-idx) 
                        current))
               (- inner-idx 1))
              (else
                (vector-set! vector (- inner-idx 1) (vector-ref vector inner-idx))
                (inner-loop (+ inner-idx 1))))))
          current)
        (if (> outer-idx 0)
          (outer-loop (- outer-idx 1)))))

(define (insertion-sort-reversed vector <<?)
  (define (>=? x y) (not (<<? x y)))
  (let outer-loop
    ((outer-idx 1))
      (let ((current (vector-ref vector outer-idx)))
        (vector-set!
          vector
          (let inner-loop
            ((inner-idx (- outer-idx 1)))
            (cond
              ((or (= inner-idx -1)
                   (<<? (vector-ref vector inner-idx) current))
               (+ inner-idx 1))
              (else
                (vector-set! vector (+ inner-idx 1) (vector-ref vector inner-idx))
                (inner-loop (- inner-idx 1)))))
          current)
        (if (< outer-idx (- (vector-length vector) 1))
        (outer-loop (+ outer-idx 1))))))

(define vec (vector 5 2 8 1 9 3))
(insertion-sort-reversed vec <)
(display vec)
