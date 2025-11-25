#lang r7rs

(import (scheme base)
        (scheme write))

(define (add-to-first! l e)
  (set! l (cons e l)))

(define (do-something)
  (define l '(2 3 4))
  (add-to-first! l 1)
  (display l))

(do-something)
