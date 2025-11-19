(define (all-but-first-n l n)
  (let iterate
    ((current l)
     (counter n))
    (if (or (= counter 0)
            (null? current))
      current
      (iterate (cdr current) (- counter 1)))))

(define (all-but-first-n l n)
  (define (iterate current counter)
    (if (or (= counter 0)
            (null? current))
        current
        (iterate (cdr current) (- counter 1))))
  (iterate l n))

