#lang r7rs

(import (scheme base)
        (scheme write)
        (prefix (a-d tree binary-tree) tree:))

; ipv meest L kind te binden in R subtree
; dus: door meest R kind van L subtree te binden

(define (find-rightmost deleted parent child! child)
  (if (tree:null-tree? (tree:right child))
      (begin 
        (tree:value! deleted (tree:value child))
        (child! parent (tree:left child)))
      (find-rightmost deleted child 
                     tree:right!
                     (tree:right child))))

(define (delete-node parent child! child)
  (cond
    ((tree:null-tree? (tree:left child))
     (child! parent (tree:right child)))
    ((tree:null-tree? (tree:right child))
     (child! parent (tree:left child)))
    (else
     (find-rightmost child
                     child
                     tree:left!
                     (tree:left child)))))