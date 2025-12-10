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

(define (depth tree)
  (cond ((bt:null-tree? tree) 0)
        (else (+ 1 (max (depth (bt:left tree))
                        (depth (bt:right tree)))))))

(define (height tree)
  (- (depth tree) 1))

(display (height testboom)) (newline)

(define (count-nodes tree)
  (cond ((bt:null-tree? tree) 0)
        (else (+ 1
                 (count-nodes (bt:left tree))
                 (count-nodes (bt:right tree))))))

(display (count-nodes testboom))