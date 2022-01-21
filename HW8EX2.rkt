;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW8EX2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Homework 8
;Exercise 2

(define LOS-0 '())
(define LOS-1  (list "hello" "hello" "world"))
(define LOS-2  (list "hello"))
(define LOS-3  (list "hello" "hello" "world" "world"))
(define LOS-4  (list "I" "really" "don't" "really" "know"))
 
;word-counts: [List-of String] -> [List-of Numbers]
;Counts the frequency of every word in a text.
(check-expect (word-counts LOS-0) (list))
(check-expect (word-counts LOS-1) (list 2 2 1))
(check-expect (word-counts LOS-2) (list 1))
(check-expect (word-counts LOS-3) (list 2 2 2 2))
(check-expect (word-counts LOS-4) (list 1 2 1 2 1))

(define (word-counts los)
  (local
    [(define (counter str)
       (local
         [(define (correlate unit)
            (if (string=? str unit) 1 0))]
         (foldr + 0 (map correlate los))))]
    (map counter los)))




(define (missing-locations atlas)
(local
  [(define lo-co (filter country? atlas))
   (define lo-ci (filter city?    atlas))
   (define (cityhascountry? loco-unit)
     (string=? (country-city unit)  
  (local
  [(define (fxn unit)
     (cond [(city? unit)    (not (ormap cityhascountry? lo-co))]
           [(country? unit) (not (ormap countryhascity? lo-ci))]     
  (filter fxn atlas)

; WHat does missing-loco do?
     ; Filters every element through a function F1
     ; F1 checks if unit is city or country (OK)
     ; if unit is a city, it checks with a list of countries with an
     ; ormap to check if it does have a country matching
     ; vice-versa if the other thing