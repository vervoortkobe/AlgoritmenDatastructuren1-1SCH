; O(m)
(define (subtract n m)
  (if (= m 0)
    n
    (subtract (- n 1) (- m 1))))

; O(min (l1, l2))
(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
    '()
    (cons (cons (car l1)
                (car l2))
          (zip (cdr l1) (cdr l2)))))
