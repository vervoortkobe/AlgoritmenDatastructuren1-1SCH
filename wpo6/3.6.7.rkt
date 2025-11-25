#lang r7rs

(import (scheme base)
        (scheme write))

(define (find-ternary! slst key)
  (define ==? (equality slst))
  (define <<? (lesser slst))
  (define vect (storage slst))
  (define leng (size slst))
  (define (ternary-search left right)
    (if (<= left right)
        (let* ((third (quotient (- right left) 3))
               (mid1 (+ left third))
               (mid2 (- right third)))
          (cond
            ((==? (vector-ref vect mid1) key)
             (current! slst mid1))
            ((==? (vector-ref vect mid2) key)
             (current! slst mid2))
            ((<<<? (vector-ref vect mid1) key)
             (ternary-search (+ mid1 1) (- mid2 1)))
            ((<<<? (vector-ref vect mid2) key)
             (ternary-search (+ mid2 1) right))
            (else
             (ternary-search left (- mid1 1)))))
        (current! slst -1)))
  (ternary-search 0 (- leng 1))
  slst)

     ; elk werk dat je bespaart, wordt teniet gedaan, omdat je elke stap meer werk hebt tov binary-search