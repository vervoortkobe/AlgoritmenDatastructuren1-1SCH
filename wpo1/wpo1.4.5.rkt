#lang r7rs

(import (scheme base)
        (scheme write)
        (scheme char)
        (only (scheme r5rs) modulo)
        (prefix (a-d dictionary unordered external-chaining) dic:))

;; ============================================
;; Hash function for strings
;; ============================================
(define (string-hash str)
  (let loop ((chars (string->list str))
             (hash 0))
    (if (null? chars)
        (abs hash)
        (loop (cdr chars)
              (+ (* hash 31) (char->integer (car chars)))))))

;; ============================================
;; Helper function to display results
;; ============================================
(define (display-line . args)
  (for-each display args)
  (newline))

;; ============================================
;; 1. Dutch-English dictionary (single translation)
;; K: String, V: String
;; ============================================
(define dutch-english-single 
  (dic:new string=? 100 string-hash))

(dic:insert! dutch-english-single "huis" "house")
(dic:insert! dutch-english-single "boek" "book")
(dic:insert! dutch-english-single "water" "water")
(dic:insert! dutch-english-single "kat" "cat")
(dic:insert! dutch-english-single "hond" "dog")

(display-line "1. Dutch-English (single translation):")
(display-line "  huis -> " (dic:find dutch-english-single "huis"))
(display-line "  boek -> " (dic:find dutch-english-single "boek"))
(display-line "  kat -> " (dic:find dutch-english-single "kat"))
(newline)


;; ============================================
;; 2. Dutch-English dictionary (multiple translations)
;; K: String, V: List of Strings
;; ============================================
(define dutch-english-multiple 
  (dic:new string=? 100 string-hash))

(dic:insert! dutch-english-multiple "bank" '("bank" "couch" "bench"))
(dic:insert! dutch-english-multiple "boot" '("boat" "boot"))
(dic:insert! dutch-english-multiple "vleugel" '("wing" "grand piano"))
(dic:insert! dutch-english-multiple "raam" '("window" "frame"))

(display-line "2. Dutch-English (multiple translations):")
(display-line "  bank -> " (dic:find dutch-english-multiple "bank"))
(display-line "  vleugel -> " (dic:find dutch-english-multiple "vleugel"))
(display-line "  boot -> " (dic:find dutch-english-multiple "boot"))
(newline)


;; ============================================
;; 3. Students with remaining credits
;; K: String, V: Integer (number of credits)
;; ============================================
(define student-credits 
  (dic:new string=? 100 string-hash))

(dic:insert! student-credits "Jan Janssen" 45)
(dic:insert! student-credits "Marie Curie" 30)
(dic:insert! student-credits "Albert Einstein" 15)
(dic:insert! student-credits "Ada Lovelace" 60)

(display-line "3. Students with remaining credits:")
(display-line "  Jan Janssen needs " 
              (dic:find student-credits "Jan Janssen") 
              " credits")
(display-line "  Marie Curie needs " 
              (dic:find student-credits "Marie Curie") 
              " credits")
(display-line "  Ada Lovelace needs " 
              (dic:find student-credits "Ada Lovelace") 
              " credits")
(newline)


;; ============================================
;; 4. Students with gender information
;; K: String, V: Boolean (true = male, false = female)
;; ============================================
(define student-gender 
  (dic:new string=? 100 string-hash))

(dic:insert! student-gender "Jan Janssen" #t)
(dic:insert! student-gender "Marie Curie" #f)
(dic:insert! student-gender "Albert Einstein" #t)
(dic:insert! student-gender "Ada Lovelace" #f)

(display-line "4. Students with gender:")
(display-line "  Jan Janssen is male: " 
              (if (dic:find student-gender "Jan Janssen") "yes" "no"))
(display-line "  Marie Curie is male: " 
              (if (dic:find student-gender "Marie Curie") "yes" "no"))
(display-line "  Ada Lovelace is male: " 
              (if (dic:find student-gender "Ada Lovelace") "yes" "no"))
(newline)


;; ============================================
;; 5. Students with study programs (nested dictionary)
;; K: String, V: Dictionary (K2: String (course), V2: Integer (mark))
;; ============================================
(define student-programs 
  (dic:new string=? 100 string-hash))

;; Create study program for Jan
(define jan-program 
  (dic:new string=? 50 string-hash))
(dic:insert! jan-program "Mathematics" 85)
(dic:insert! jan-program "Physics" 78)
(dic:insert! jan-program "Chemistry" 92)
(dic:insert! jan-program "Computer Science" 88)
(dic:insert! student-programs "Jan Janssen" jan-program)

;; Create study program for Marie
(define marie-program 
  (dic:new string=? 50 string-hash))
(dic:insert! marie-program "Mathematics" 95)
(dic:insert! marie-program "Physics" 98)
(dic:insert! marie-program "Chemistry" 88)
(dic:insert! marie-program "Biology" 91)
(dic:insert! student-programs "Marie Curie" marie-program)

;; Create study program for Albert
(define albert-program 
  (dic:new string=? 50 string-hash))
(dic:insert! albert-program "Mathematics" 100)
(dic:insert! albert-program "Physics" 99)
(dic:insert! albert-program "Philosophy" 87)
(dic:insert! student-programs "Albert Einstein" albert-program)

(display-line "5. Students with study programs:")
(define jan-courses (dic:find student-programs "Jan Janssen"))
(display-line "  Jan's Mathematics grade: " 
              (dic:find jan-courses "Mathematics"))
(display-line "  Jan's Physics grade: " 
              (dic:find jan-courses "Physics"))
(display-line "  Jan's Computer Science grade: " 
              (dic:find jan-courses "Computer Science"))

(define marie-courses (dic:find student-programs "Marie Curie"))
(display-line "  Marie's Mathematics grade: " 
              (dic:find marie-courses "Mathematics"))
(display-line "  Marie's Chemistry grade: " 
              (dic:find marie-courses "Chemistry"))

(define albert-courses (dic:find student-programs "Albert Einstein"))
(display-line "  Albert's Physics grade: " 
              (dic:find albert-courses "Physics"))
(display-line "  Albert's Philosophy grade: " 
              (dic:find albert-courses "Philosophy"))