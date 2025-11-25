#lang r7rs

(import (scheme base)
        (scheme write)
        (prefix (a-d positional-list vector-positional-list) plist:)
        (prefix (a-d pattern-matching quicksearch) pmatch:))

(define l (plist:new eq?))
(plist:add-before! l "and")
(plist:add-after! l "me")
(plist:add-before! l "to" (plist:last l))
(plist:add-after! l "goodday" (plist:first l))
(plist:add-before! l "hello")
(plist:add-after! l "world" (plist:first l))

(define (pair-eq? p1 p2)
  (eq? (cdr p1) (cdr p2)))

; stap 1
(define m (plist:map l (lambda (v) (cons v (string-length v))) pair-eq?))
(define positie-van-woord-lengte-7 (plist:find m (cons 'ignored 7)))
(display (plist:peek m positie-van-woord-lengte-7))