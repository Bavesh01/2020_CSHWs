;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 1
; subset-interval? : Number Number Number Number -> Boolean
; Returns whether the set of first two numbers is a subset of the second two.
(check-expect (subset-interval? 2 3 1 4) #t)
(check-expect (subset-interval? 3 6 3 6) #t)
(check-expect (subset-interval? 4 5 6 7) #f)
(check-expect (subset-interval? 8 10 7 9) #f)
; more check expects needed
(define (subset-interval? a b c d)
  (and (>= b a) (>= d c) (>= a c) (>= d b)))

; Exercise 2
; sort-three : Number Number Number -> String
; Returns a sequence of ascending numbers as a string in the form "[...]"
(check-expect (sort-three 3 1 2) "[1,2,3]")
(check-expect (sort-three 4 4 -6) "[-6,4,4]")
(check-expect (sort-three -4 3 -4) "[-4,-4,3]")
(check-expect (sort-three 0 2 3) "[0,2,3]")
(check-expect (sort-three 5 4 -6) "[-6,4,5]")
(check-expect (sort-three -5 -10 6) "[-10,-5,6]")
(check-expect (sort-three 4 6 5) "[4,5,6]")
(define (sort-three a b c)
  (cond [(and (>= c b) (>= b a)) (3nums->string a b c)]
        [(and (>= c a) (>= a b)) (3nums->string b a c)]
        [(and (>= b a) (>= a c)) (3nums->string c a b)]
        [(and (>= b c) (>= c a)) (3nums->string a c b)]
        [(and (>= a c) (>= c b)) (3nums->string b c a)]
        [(and (>= a b) (>= b c)) (3nums->string c b a)]))


; 3nums->string : Number Number Number -> String
; Returns a simple sequence of numbers as a string in the form "[...]"
(check-expect (3nums->string 3 1 2) "[3,1,2]")
(check-expect (3nums->string 4 4 -6) "[4,4,-6]")
(define (3nums->string x y z)
  (string-append "[" (number->string x) ","
                 (number->string y) ","
                 (number->string z) "]"))

; Exercise 3
; => : Boolean Boolean -> Boolean
; Returns whether or not the first Boolean implies the second Boolean
(check-expect (=> (> 4 2) (> 4 1)) #t)
(check-expect (=> (> 3 3) (> 4 4)) #t)
(check-expect (=> (> 6 3) (> 0 4)) #f)
(check-expect (=> (> 2 3) (> 7 4)) #t)
(define (=> x y)
  (if x
      (if y #t #f)
      (if y #t #t)))

; So, by my implementation, the answer for
; "If Boston is the capital of the moon, there will be two exams in Fundies I."
; is true since the first statement is false and the second statement is true.

; Exercise 4
; CombinedMajor is one of these strings:-
;- "CS and Linguistics"
;- "CS and Biology"
;- "CS and Cognitive Psychology"
;- "CS and Mathematics"
;- "DS and Biochemistry"
;- "DS and Biology"
;- "DS and Ecology and Evolutionary Biology"
;- "DS and Mathematics"
; Some of the combined majors provided by CCIS and COS
(define CM-A "CS and Linguistics")
(define CM-B "CS and Biology")
(define CM-C "DS and Biochemistry")

; cm-templ -> CombinedMajor -> ...
#;
(define (cm-templ cm)
  (... (cond [(string=? cm CM-A) ...]
             [(string=? cm CM-B) ...]
             [(string=? cm CM-C) ...])...))

; cs-or-ds : CombinedMajor -> String
; returns whether the CombinedMajor belongs to CS or DS.
(check-expect (cs-or-ds "CS and Linguistics") "CS")
(check-expect (cs-or-ds "CS and Mathematics") "CS")
(check-expect (cs-or-ds "CS and Biology") "CS")
(check-expect (cs-or-ds "CS and Cognitive Psychology") "CS")
(check-expect (cs-or-ds "DS and Biology") "DS")
(check-expect (cs-or-ds "DS and Mathematics") "DS")
(check-expect (cs-or-ds "DS and Ecology and Evolutionary Biology") "DS")
(check-expect (cs-or-ds "DS and Biochemistry") "DS")
; check expects
(define (cs-or-ds cm)
  (cond [(string=? cm "CS and Linguistics") "CS"]
        [(string=? cm "CS and Biology") "CS"]
        [(string=? cm "CS and Cognitive Psychology") "CS"]
        [(string=? cm "CS and Mathematics") "CS"]
        [(string=? cm "DS and Biochemistry") "DS"]
        [(string=? cm "DS and Biology") "DS"]
        [(string=? cm "DS and Ecology and Evolutionary Biology") "DS"]
        [(string=? cm "DS and Mathematics") "DS"]))

; cs-or-ds-v2 : CombinedMajor -> String
; returns whether the CombinedMajor belongs to CS or DS.
; check expects
(check-expect (cs-or-ds-v2 "CS and Linguistics") "CS")
(check-expect (cs-or-ds-v2 "CS and Mathematics") "CS")
(check-expect (cs-or-ds-v2 "DS and Biology") "DS")
(check-expect (cs-or-ds-v2 "DS and Mathematics") "DS")
(define (cs-or-ds-v2 cm)
  (cond [(string=? (substring cm 0 2) "CS") "CS"]
        [(string=? (substring cm 0 2) "DS") "DS"]))

; alternative : CombinedMajor -> CombinedMajor
; returns alternate khoury combined major with the same non-Khoury program.
; check-expects
(check-error (alternative "sample") ERROR)
(check-expect (alternative "CS and Biology") "DS and Biology")
(check-expect (alternative "DS and Biology") "CS and Biology")
(check-expect (alternative "CS and Mathematics") "DS and Mathematics")
(check-expect (alternative "DS and Mathematics") "CS and Mathematics")
(define ERROR "No alternative! Try again.")
(define (alternative cm)
  (cond [(string=? cm "CS and Biology") "DS and Biology"]
        [(string=? cm "DS and Biology") "CS and Biology"]
        [(string=? cm "CS and Mathematics") "DS and Mathematics"]
        [(string=? cm "DS and Mathematics") "CS and Mathematics"]
        [else (error ERROR)]))

; Exercise 5
(define BASE 2)
(define LIMIT 10000000)
(define SCENE (rectangle 900 600 "solid" "black"))
(define SIZE 40)
(define COLOR "light blue")

(require 2htdp/image)
(require 2htdp/universe)
(require slideshow/text)

; powers-of-base: Number -> Number
; Returns the value of succeeding exponent function.
(define (powers-of-base n)
  (big-bang n
    [to-draw draw-number] ; Number -> Image
    [on-key raise-number] ; Number KeyEvent -> Number
    [stop-when end-program? end-program])) ; Number -> Boolean

; draw-number: Number -> Image
; Draws the current state i.e. displays the value of the BASE raised to the exponent
(check-expect (draw-number 3) (overlay (text (string-append "2^"
                                                            (number->string 3)
                                                            "="
                                                            (number->string (expt BASE 3)))
                                             SIZE COLOR) SCENE))
(check-expect (draw-number 4) (overlay (text (string-append "2^"
                                                            (number->string 4)
                                                            "="
                                                            (number->string (expt BASE 4)))
                                             SIZE COLOR) SCENE))
(define (draw-number n)
  (overlay (text (string-append "2^"
                                (number->string n)
                                "="
                                (number->string (expt BASE n))) SIZE COLOR) SCENE))  

; raise-number: Number -> Number
; Given number as exponent value, returns the succeeding number
(check-expect (raise-number 2 "r") 3)
(check-expect (raise-number 99 "b") 100)
(define (raise-number n key)
  (+ 1 n))
   
; end-program?: Number -> Boolean
; Checks whether the number input exceeds the Limit
(check-expect (end-program? 9999) #true)
(check-expect (end-program? 3) #false)
(define (end-program? n)
  (> (expt BASE n) LIMIT))

; end-program: Number -> Image
; Shows an ending screen
(check-expect (end-program 10) (overlay (text (string-append "Value exceeds "
                                                             (number->string LIMIT)
                                                             "!")
                                              SIZE COLOR) SCENE))
(check-expect (end-program 20) (overlay (text (string-append "Value exceeds "
                                                             (number->string LIMIT)
                                                             "!")
                                              SIZE COLOR) SCENE))
(define (end-program n)
  (overlay (text (string-append "Value exceeds "
                                (number->string LIMIT)
                                "!")
                 SIZE COLOR) SCENE))

