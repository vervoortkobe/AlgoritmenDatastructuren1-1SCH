#lang r7rs

; new      : number × number → pair
; numer    : number → number
; denom    : pair → number
; fraction?: any → boolean
; +        : pair × pair → pair
; -        : pair × pair → pair
; *        : pair × pair → pair
; /        : pair × pair → pair

(define-library (fraction)
  (export new fraction? numer denom + - * /)
  (import (rename (scheme base) 
                (+ base:+)
                (- base:-)
                (* base:*)
                (/ base:/))
        (scheme write)
        (scheme cxr))
  (begin
    (define fraction-tag 'fraction)
    
    (define (new n d)
      (list fraction-tag n d))
    
    (define (fraction? any)
      (and (list? any)
           (= (length any) 3)
           (eq? (car any) fraction-tag)))

    (define (numer f)
      (cadr f))
    
    (define (denom f)
      (caddr f))
    
    (define (+ f1 f2)
      (new (base:+ (base:* (numer f1) (denom f2))
                   (base:* (numer f2) (denom f1)))
           (base:* (denom f1) (denom f2))))

    (define (- f1 f2)
      (new (base:- (base:* (numer f1) (denom f2))
                   (base:* (numer f2) (denom f1)))
           (base:* (denom f1) (denom f2))))
    (define (* f1 f2)
      (new (base:* (numer f1) (numer f2))
           (base:* (denom f1) (denom f2))))
    
    (define (/ f1 f2)
      (new (base:* (numer f1) (denom f2))
           (base:* (denom f1) (numer f2))))
    )
  )