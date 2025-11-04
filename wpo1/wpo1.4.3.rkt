#lang r7rs

(import (rename (scheme base) (= base:=) (* base:*))
        (scheme write)
        (prefix (a-d fraction fraction) fraction:))


(define (= f1 f2)
  (and (fraction:fraction? f1)
       (fraction:fraction? f2)
       (base:= (base:* (fraction:numer f1) (fraction:denom f2))
               (base:* (fraction:numer f2) (fraction:denom f1)))))