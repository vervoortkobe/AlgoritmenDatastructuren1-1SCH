#lang r7rs
(import (scheme base)
        (scheme write))

(define (char->number-value char)
  (let ((ascii-value (char->integer char)))
    (- ascii-value 48)))

(display (char->number-value #\0))
(display (char->number-value #\5))

(define (my-string->number str)
  (define (iter position result)
    (if (= position (string-length str))
        result
        (let* ((char (string-ref str position))
               (val (char->number-value char)))
          (iter (+ position 1) (+ (* result 10) val))))
    (iter 0 0))
  (newline)
  )

(display (number? (my-string->number "1234")))
(display (my-string->number "1234"))