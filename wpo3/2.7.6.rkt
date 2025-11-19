#lang r7rs

(import (scheme base) (scheme write))

(begin
  (define (match t p)
    (define n-t (string-length t))
    (define n-p (string-length p))

    (define results '())
      
    (let loop ((i-t 0))
      (if (> i-t (- n-t n-p))
          (reverse results)
          (let match-at-pos ((i-p 0))
            (if (> i-p (- n-p 1))
                (begin
                  (set! results (cons i-t results))
                  (loop (+ i-t 1)))
                (if (eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
                    (match-at-pos (+ i-p 1))
                    (loop (+ i-t 1)))))))))

(display (match "bababxyz" "bab"))