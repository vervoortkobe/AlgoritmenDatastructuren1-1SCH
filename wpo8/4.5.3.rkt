#lang r7rs

(import (scheme base)
        (scheme write)
        (prefix (a-d queue list) queue:))

(define (josephus lst m)
  (define queue (queue:new))
  (let prepare-queue ((current lst))
    (when (not (null? current))
      (queue:enqueue! queue (car current))
      (prepare-queue (cdr current))))
  (let process-queue ((i 1))
    (let ((element (queue:serve! queue)))
      (cond ((queue:empty? queue) element)
            ((= i m) (process-queue 1))
            (else (queue:enqueue! queue element)
                  (process-queue (+ i 1)))))))

(display (josephus '(a b c d e) 3))