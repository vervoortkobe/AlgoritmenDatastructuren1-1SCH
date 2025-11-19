; O(n^2)
(define (all-i-to-j n)
  (define (i-to-j i j)
    (if (= j 0)
      1
      (* i (i-to-j i (- j 1)))))
  (define (sum i)
    (if (= i 0)
      0
      (+ (sum (- i 1)) (i-to-j i i))))
  (sum n))
