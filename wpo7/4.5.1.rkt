#lang r7rs

(import (scheme base)
        (scheme write)
        (prefix (a-d stack vectorial) stack:)
        (prefix (a-d queue list) queue:))

(define (pop-all! stack)
  (let loop ((lst '()))
    (if (stack:empty? stack)
        lst
        (loop (cons (stack:pop! stack) lst)))))

(define (postfix-eval expr)
  (define stack (stack:new))
    (let loop ((current expr))
      (if (null? current)
          (stack:pop! stack)
          (let ((element (car current)))
            (if (procedure? element)
                (stack:push! stack (apply element (pop-all! stack)))
                (stack:push! stack element))
            (loop (cdr current))))))

(display (postfix-eval (list 5 6 + 7 -))) ; -> 4