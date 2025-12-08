#lang r7rs

(import (scheme base)
        (scheme write))

(define (split lst)
  (define l (length lst))
  (define (iter teller linkerhelft rechterhelft)
    (if (< teller (/ l 2))
        (iter (+ teller 1)
              (cons (car rechterhelft) linkerhelft) ; omdat we vooraan cons'en wordt de linkerhelft omgedraaid maar dit is geen probleem!
              (cdr rechterhelft))
        (cons linkerhelft rechterhelft)))
  (iter 0 '() lst))

(define (merge-gesorteerd lst1 lst2 <<?)
  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else (let ((car1 (car lst1))
                    (car2 (car lst2)))
                (if (<<? car1 car2)
                    (cons car1 (merge-gesorteerd (cdr lst1) lst2 <<?))
                    (cons car2 (merge-gesorteerd lst1 (cdr lst2) <<?)))))))

(define (merge-sort lst <<?)
  (if (> (length lst) 1)
      (let* ((helften (split lst))
             (linkerhelft (car helften))
             (rechterhelft (cdr helften)))
        (merge-gesorteerd (merge-sort linkerhelft <<?)
                          (merge-sort rechterhelft <<?)
                          <<?))
      lst))

(display (merge-sort '(10 2 8 1 9 3 7 4 6 5) <))
