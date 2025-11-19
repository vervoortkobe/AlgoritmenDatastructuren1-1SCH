#lang r7rs
(import (scheme base)
        (scheme write))

(define (my-ascii-chars char)
  (char->integer char))

(display (my-ascii-chars #\a))