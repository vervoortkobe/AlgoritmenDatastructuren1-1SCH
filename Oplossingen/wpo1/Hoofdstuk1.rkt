#lang r7rs

;; Alle nodige imports voor de oefeningen van hoofdstuk 1 zijn hier opgelijst.
(import (except (scheme base) map)
        (scheme write)
        (prefix (Hoofdstuk1 fraction) fraction:) ; ex. 3
        (prefix (Hoofdstuk1 disk) disk:) ; ex. 4
        (scheme inexact)) ; inexact maths library, used for sqrt procedure

; Exercise 1

; Specify the procedural type of the following built-in Scheme procedures: cons,
; car, cdr, vector-ref, vector-set!, member.  You can use the following data
; types: any, pair, vector, number, boolean and void.  You can also use
; singleton sets such as {#f}.

; -> The procedural type of cons        is (any any -> pair)
; -> The procedural type of car         is (pair -> any)
; -> The procedural type of cdr         is (pair -> any)
; -> The procedural type of vector-ref  is (vector number -> any)
; -> The procedural type of vector-set! is (vector number any -> ∅)
; -> The procedural type of member      is (any pair -> pair ∪ {#f})

; Exercise 2

; Specify the procedural type of the following higher-order procedures.  You can
; use the same data types as in the previous exercise.

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))
;
; -> The procedural type of (map f l)
;             is ((any -> any) pair -> pair)

(define (sum a b term next)
  (if (> a b)
      0
      (+ (term a)
         (sum (next a) b term next))))
;
; -> The procedural type of (sum a b term next)
;             is (number number (number -> number) (number -> number) -> number)

(define (compose f g)
  (lambda (x)
    (f (g x))))
;
; -> The procedural type of (compose f g)
;             is ((any -> any) (any -> any) -> (any -> any))

; Exercise 3.
; Analogous to the complex ADT, let's define the fraction ADT.

; First, formulate the ADT itself, i.e., specify all procedures along with their procedure type.
; -> See fraction.rkt.

; Second, implement the ADT in the procedural style as a Scheme library.
; -> See fraction.rkt.

; Third, write a procedure = that uses the ADT in order to verify whether or not to fractions are equal.  You are not allowed to add = to your library.
; -> Mogelijke oplossing:
(define (fraction:= f1 f2)
  (= (/ (fraction:numerator f1) (fraction:denominator f1))
     (/ (fraction:numerator f2) (fraction:denominator f2))))
; -> Alternatief:
;    (define (fraction:= f1 f2)
;      (let ((difference (fraction:- f1 f2)))
;        (= (fraction:numerator difference) 0)))

(define f1 (fraction:new 1 2)) (display f1) (newline)
(define f2 (fraction:new 2 4)) (display f2) (newline)
(define f3 (fraction:new 3 4)) (display f3) (newline)

(display (fraction:= f1 f1)) (newline)
(display (fraction:= f1 f2)) (newline)
(display (fraction:= f1 f3)) (newline)

; Fourth, reimplement the constructor such that rationals are always represented in reduced form.  Does this reimplementation affect your code for =?
; -> Zie fraction.rkt.  Deze herimplementatie beïnvloedt onze code voor = niet.

; Exercise 4.
; The software company KidSoft is creating a drawing program for children between 8 and 12 years old.
; One of the features of the program consists of creating colorful disks.
; We can think of a disk as a circle that is filled with a certain color.
; The circle can be thought of as a center (represented by two numbers that correspond to 2D-coordinates) and a radius.

; First, formulate the ADT disk.  Implement the constructor and the accessors in the procedural style.
; -> Zie disk.rkt.

; Second, implement (external to the ADT library) the following additional operations: concentric?, same-size?, same-color?, identical?.
(define (concentric? disk1 disk2)
  (point:= (disk:centre disk1) (disk:centre disk2)))

(define (point:= p1 p2)
  (and (= (disk:x-coordinate p1) (disk:x-coordinate p2))
       (= (disk:y-coordinate p1) (disk:y-coordinate p2))))

(define (same-size? disk1 disk2)
  (= (disk:radius disk1) (disk:radius disk2)))

(define (same-color? disk1 disk2)
  (equal? (disk:color disk1) (disk:color disk2)))

(define (identical? disk1 disk2)
  (and (concentric? disk1 disk2)
       (same-size? disk1 disk2)
       (same-color? disk1 disk2)))

; Third, implement the additional operations: subdisk?, intersects?, touches?.
(define (subdisk? disk1 disk2)
  (<= (distance (disk:centre disk1) (disk:centre disk2))
      (- (disk:radius disk2) (disk:radius disk1))))

(define (distance p1 p2)
  (sqrt (+ (sqr (- (disk:x-coordinate p1) (disk:x-coordinate p2)))
           (sqr (- (disk:y-coordinate p1) (disk:y-coordinate p2))))))

(define (sqr x)
  (* x x))

(define (intersects? disk1 disk2)
  (and (< (distance (disk:centre disk1) (disk:centre disk2))
          (+ (disk:radius disk1) (disk:radius disk2)))
       (not (subdisk? disk1 disk2))
       (not (subdisk? disk2 disk1))))

(define (touches? disk1 disk2)
  (or (= (distance (disk:centre disk1) (disk:centre disk2))
         (+ (disk:radius disk1) (disk:radius disk2)))
      (= (distance (disk:centre disk1) (disk:centre disk2))
         (- (disk:radius disk1) (disk:radius disk2)))))

; Exercise 5.
; Consider the ADT dictionary<K,V> and suppose that we want to use an implementation of the ADT in the following applications.  Formally specify K and V for all cases.

; A dictionary Dutch-English that maps a Dutch word onto its only translation in English.
; -> dictionary<string,string>

; A dictionary Dutch-English that maps a Dutch word onto a series of possible translations in English.
; -> dictionary<string,pair>

; A list of students that associates a student's name with the number of credits he (or she) still has to collect in order to get a bachelor's degree.
; -> dictionary<string,number>

; A list of students that associates a student's name with the fact whether or not the student is male.
; -> dictionary<string,boolean>

; A list of students that associates a student with his or her study program.  The study program is a mapping that associates course names with the mark obtained by the student for that particular course.
; -> dictionary<string,dictionary<string,number>>

; Exercise 6

(define (last-of-list lst)
  (cond ((null? lst)
         '())
        ((null? (cdr lst))
         (car lst))
        (else
         (last-of-list (cdr lst)))))

; - last-of-list:
; This procedure uses recursion, hence we apply our rule of thumb:
; we will multiply the work of the body (excluding the recursive call) by the number of recursive steps.
;
; Worst case performance:
; - Work of the body = O(1). The body consists of a single expression (cond) which has a O(1) worst case performance. All elements of the cond special form are O(1), thus the heaviest step remains O(1) (excluding the recursive call which is in tail position in the else branch).
; - Number of recursive steps = O(n), since for an arbitrary number of elements in the list, n, we will have to walk to end of the list by omitting the first element at each recursive call with cdr. In other words: the number of recursive steps grows linearly when increasing n.
; - Total performance = O(1 * n) =  O(n), where n is the size of the list.
;
; Best case performance:
; - Work of the body = Ω(1), since all elements of the cond special form are Ω(1), the lightest step remains Ω(1).
; - Number of recursive steps = Ω(n); For any arbitrary input of size n, you'll always have to pass through the whole list. Stating that this could be Ω(1), when the list has size 1 or 0, is therefore incorrect!
; - Total performance =  Ω(1 * n) =  Ω(n)

(define (last-of-vector vctr)
  (let ((length (vector-length vctr)))
    (if (< length 1)
        '()
        (vector-ref vctr (- length 1)))))

; - last-of-vector:
; Since vectors have a fixed length, we can use the length of the vector to access the last element quickly.
; This procedure has no recursion, so we simply sum the duration of each expression of its body while analysing the performance.
;
; Worst case performance = O(1), since each part of the let special form has a O(1) performance (calling vector-length, executing the if etc.).
; Hence, the sum remains O(1).

; Best case performance = Ω(1), since each part of the let special form has a Ω(1) performance.

; Given that both best and worst case performance are from the same order, we can even state that:
; - last-of-list has a performance of Θ(n).
; - last-of-vector has a performance of Θ(1).

; Exercise 7 - Iterative implementation of length-of-list

(define (length-of-list lst)
  (define (iter-length count rest-of-lst) ; defining the help procedure is O(1), the execution however would cost O(n), where n is the length of rest-of-lst
    (cond ((null? rest-of-lst)
           count)
          (else
           (iter-length (+ count 1) (cdr rest-of-lst)))))
  (iter-length 0 lst)) ; calling (and thus executing) the help procedure is O(n), where n is the length of the actual parameter, lst

(define (length-of-list-rec lst)
  (if (null? lst)
      0
      (+ 1 (length-of-list-rec (cdr lst)))))

; - length-of-list:
; The actual analysis for the iterative and the recursive version is slightly different, but the outcome will be the same.
;
; Worst case performance = O(n), where n is the size of the list, since we have to walk until the end of the list to count all the elements.
;
; Best case performance = Ω(n), since for any arbitrary input of size n, you'll always have to pass through the whole list. Stating that this could be Ω(1) when the list has size 1 or 0 is therefore incorrect.

(define (length-of-vector vctr)
  (vector-length vctr))

; - length-of-vector:
;
; Worst case performance = O(1)
;   Since vectors have a fixed length, we can simply obtain the length of the vector in O(1).
;
; Best case performance = Ω(1)

; Given that both best and worst case performance are from the same order, we can even state that:
; - length-of-list has a performance of Θ(n)
; - length-of-vector has a performance of Θ(1)

; Exercise 8

(define (all-but-first-n l n)
  (define (iterate current counter)
    (if (or (= counter 0)
            (null? current))
        current
        (iterate (cdr current) (- counter 1))))
  (iterate l n))

; Exercise 9

; subtract: O(m)
; zip:      O(min(length(l1), length(l2)))

; Exercise 10

; i-to-j:     O(j)
; sum:        i * (i-to-j i i) = i * O(i) = O(i^2)
; all-i-to-j: (sum n) = O(n^2)
