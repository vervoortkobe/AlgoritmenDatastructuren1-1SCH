#lang r7rs
(import (scheme base)
        (scheme write)
        (prefix (a-d pattern-matching quicksearch) pmatch:)
        (prefix (a-d positional-list single-linked-positional-list) plist:))

(define (intersection p1 p2)
  (define result (plist:new eq?))
  (if (or (plist:empty? p1)
      (plist:empty? p2))
      result
      (let loop ((current-pos (plist:first p1)))
        (let ((current-val (plist:peek p1 current-pos)))
          (when (plist:find p2 current-val)
            (plist:add-after! result current-val))
          (if (plist:has-next? p1 current-pos)
              (loop (plist:next p1 current-pos))
              result)))))
 

(define p (plist:from-scheme-list '(2 4 6 8 10 12 14) eq?))
(define q (plist:from-scheme-list '(3 6 9 12 15 18 21) eq?))
(display "(intersection p q) ---> ") (display (intersection p q))