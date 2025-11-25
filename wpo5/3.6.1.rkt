#lang r7rs

(import (scheme base)
        (scheme write))

(define-record-type exercise-a
  (make-a field1 field2 field3)
  exercise-a?
  (field1 a-field1 a-field1!)
  (field2 a-field2)
  (field3 a-field3))

  (define x (make-a 3 eq? (vector 5 6 9 0 0 0 0 0 0 0 0 0 )))


  (define-record-type exercise-c
  (make-c field1 field2 field3)
  exercise-c?
  (field1 c-field1)
  (field2 c-field2)
  (field3 c-field3))


(let ((last-element ( cons 7 '())))
  (define l (append (list -5 -8 -1 6 2 0)
                    last-element))
  (define c (make-c last-element 3 l))
  (display c))
