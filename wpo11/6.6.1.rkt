#lang r7rs

(import (scheme base)
        (scheme write)
        (prefix (a-d tree binary-tree) bt:))

; testboom: 1
;          / \
;         2   6
;        / \
;       3   4
;            \
;             5
(define testboom
  (bt:new 1
          (bt:new 2
                  (bt:new 3
                          bt:null-tree
                          bt:null-tree)
                  (bt:new 4
                          bt:null-tree
                          (bt:new 5
                                  bt:null-tree
                                  bt:null-tree)))
          (bt:new 6
                  bt:null-tree
                  bt:null-tree)))

(define (leaf? tree)
  (and (not (bt:null-tree? tree))
       (bt:null-tree? (bt:left tree))
       (bt:null-tree? (bt:right tree))))

(define (count-leaves tree)
  (cond((bt:null-tree? tree) 0)
       ((leaf? tree) 1)
       (else (+ (count-leaves (bt:left tree))
                (count-leaves (bt:right tree))))))

(display (count-leaves testboom))