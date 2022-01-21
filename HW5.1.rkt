;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW5.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exercise 1

(define-struct moon [x v])
     
; A Moon is a (make-moon Number Number)
; the position and velocity of a moon.
;  - x is the x-position of the moon
;  - v is the x-velocity of the moon
(define MOON-1 (make-moon 10 1))
(define MOON-2 (make-moon 300 -1))
(define (moon-templ m)
  (... (moon-x m) ... (moon-v m) ...))
          
(define-struct system [moon system])
; A System is one of:
; - 0
; - (make-system Moon System)
; Representing the moons we are simulating:
; - zero moons, OR
; - the first moon and the rest of the moons
(define SYSTEM-0 0)
(define SYSTEM-1 (make-system MOON-1 SYSTEM-0))
(define SYSTEM-2 (make-system MOON-2 SYSTEM-1)) 
#;
(define (system-templ s)
  ...
  (cond [(number? s) ...]
        [(system? s) ... (moon-templ   (system-moon   s)) ...
                     (system-templ (system-system s)) ...])
  ...)

(require 2htdp/image)
(require 2htdp/universe)
     
(define SIZE 400)
(define MIDDLE (/ SIZE 2))
     
(define SUN   (circle (/ SIZE 10) "solid" "yellow"))
(define MOON  (circle (/ SIZE 10) "solid" "gray"))
(define SKY   (square SIZE "solid" "pink"))
(define SCENE (place-image SUN MIDDLE MIDDLE SKY))

     
; draw-eclipse : System -> Image 
; Draw the moons into the SCENE
(check-expect (draw-eclipse SYSTEM-0) SCENE)
(check-expect (draw-eclipse SYSTEM-2)
              (draw-moon MOON-1 (draw-moon MOON-2 SCENE)))
(define (draw-eclipse s)
  (cond [(number? s) SCENE]
        [(system? s) (draw-moon (system-moon s)
                                (draw-eclipse (system-system s)))]))
     
; draw-moon : Moon Image -> Image
; Draws a moon onto a background
(check-expect (draw-moon MOON-1 SKY)
              (place-image MOON (moon-x MOON-1) MIDDLE SKY))
(define (draw-moon moon background)
  (place-image MOON (moon-x moon) MIDDLE background))
          
; move-eclipse : System -> System
; Moves ALL moons for one tick
(check-expect (move-eclipse SYSTEM-0) SYSTEM-0)
(check-expect (move-eclipse SYSTEM-2)
              (make-system (move-moon MOON-2) 
                           (make-system (move-moon MOON-1) 0)))
(define (move-eclipse s)
  (cond [(number? s) 0]
        [(system? s) (make-system (move-moon (system-moon s))
                                  (move-eclipse (system-system s)))]))
     
; move-moon : Moon -> Moon
; Moves a single moon
(check-expect (move-moon MOON-1) (make-moon 11 1))
(check-expect (move-moon MOON-2) (make-moon 299 -1))
(define (move-moon m)
  (make-moon (+ (moon-x m) (moon-v m))
             (moon-v m)))

; eclipse : System -> System
; Runs an eclipse with certain number of moons
(define (eclipse initial-s)
  (big-bang initial-s
    [to-draw draw-eclipse]
    [on-tick move-eclipse]
    [on-key change-moon-count]))
     
(define NEW-MOON (make-moon 1 5))
     
; add-moon : System KeyEvent -> System
; Adds a moon to the system
(check-expect (add-moon SYSTEM-0 " ")     (make-system NEW-MOON 0))
(check-expect (add-moon SYSTEM-2 "right") (make-system NEW-MOON SYSTEM-2))
(define (add-moon s key)
  (cond [(number? s) (make-system NEW-MOON 0)]
        [(system? s) (make-system NEW-MOON s)]))

;a
(define NEW-MOON-1 (make-moon 0 1))
(define NEW-MOON-2 (make-moon 300 -1))
(define NEW-MOON-3 (make-moon 20 1))

(define NEW-SYSTEM-0 0)
(define NEW-SYSTEM-1 (make-system NEW-MOON-1 NEW-SYSTEM-0))
(define NEW-SYSTEM-2 (make-system NEW-MOON-2 NEW-SYSTEM-1)) 
(define NEW-SYSTEM-3 (make-system NEW-MOON-3 NEW-SYSTEM-2))

;b
; change-moon-count : System KeyEvent -> System
; Adds moons according to key strokes.
(check-expect (change-moon-count NEW-SYSTEM-3 "2")
              (make-system NEW-MOON-2 (make-system NEW-MOON-1 NEW-SYSTEM-3)))
(check-expect (change-moon-count NEW-SYSTEM-3 "3")
              (make-system NEW-MOON-3 (make-system NEW-MOON-2 (make-system NEW-MOON-1 NEW-SYSTEM-3))))
(check-expect (change-moon-count NEW-SYSTEM-3 "\b")
              (system-system NEW-SYSTEM-3))
(check-expect (change-moon-count NEW-SYSTEM-0 "\u007F") 0)
(check-expect (change-moon-count NEW-SYSTEM-1 "f")
              (make-system NEW-MOON-1 NEW-SYSTEM-1))

(define (change-moon-count sys key)
  (cond [(and (number? sys)
              (or (string=? key "\b") 
                  (string=? key "\u007F"))) 0]
        [(string=? key "2") (make-system NEW-MOON-2
                                         (make-system NEW-MOON-1 sys))]
        [(string=? key "3") (make-system NEW-MOON-3
                                         (make-system NEW-MOON-2
                                                      (make-system NEW-MOON-1 sys)))]
        [(or (string=? key "\b") 
             (string=? key "\u007F")) (system-system sys)]
        [else (make-system NEW-MOON-1 sys)]))

; Exercise 2

; A ListofString (LoS) is one of
; - '()
; - (cons String LoS)
; repr. a list of strings.
(define LOS-0 '())
(define LOS-1 (cons "hi" LOS-0))
(define LOS-2 (cons "alice" LOS-1))
(define LOS-3 (cons "hi" LOS-2))

#;
(define (los-templ los)
  ... (cond [(empty? los) ...]
            [(cons?  los) ... (first los) ... (los-templ (rest los)) ...]))

;are-there-two?: ListOfString String  -> Boolean
;determines if there two copies of s in ListOfString
(check-expect (are-there-two? LOS-3 "hi") #t)
(check-expect (are-there-two? LOS-3 "alice") #f)
(check-expect (are-there-two? LOS-0 "alice") #f)
(check-expect (are-there-two? LOS-2 "hi") #f)

(define (are-there-two? lis str)
  (cond
    [(empty? lis) #f]
    [(cons? lis) (>= (/ (string-length (similar-strings lis str))
                        (string-length str)) 2)]))

;similar-strings: LoS String -> String
;appends all the copies of str in a LoS
(check-expect (similar-strings LOS-3 "hi") "hihi")
(check-expect (similar-strings LOS-3 "alice") "alice")
(check-expect (similar-strings LOS-0 "alice") "")
(check-expect (similar-strings LOS-3 "helloworld") "")

(define (similar-strings los str)
  (cond [(empty? los) ""]
        [(equal? (first los) str)
         (string-append (first los) (similar-strings (rest los) str))]
        [else (similar-strings (rest los) str)]))

;Exercise 3

(define-struct city [name country pop])
 
; A City is a (make-city String String Nat)
; repr. the name, country, and population of the city
(define CITY-BOS (make-city "Boston"     "USA"           700000))
(define CITY-GEN (make-city "Geneva"     "Switzerland"   450000))
(define CITY-ZUR (make-city "Zurich"     "Switzerland"   450000))
(define CITY-TOK (make-city "Tokyo"      "Japan"       10000000))
(define CITY-LUC (make-city "Luckenbach" "USA"               12))
(define CITY-0   (make-city "Zero"       "Zero"               0))

;a
;city-max-2: City City -> City
;returns the city with greater population

(check-expect (city-max-2 CITY-BOS CITY-GEN) CITY-BOS)
(check-expect (city-max-2 CITY-GEN CITY-BOS) CITY-BOS)
(check-expect (city-max-2 CITY-ZUR CITY-GEN) CITY-ZUR)

(define (city-max-2 city1 city2)
  (if (< (city-pop city1) (city-pop city2))
      city2
      city1))

;b
; A ListofCity (LoC) is one of
; - '()
; - (cons City LoC)
; repr. a list of cities.
(define LOC-0 '())
(define LOC-1 (cons CITY-BOS LOC-0))
(define LOC-2 (cons CITY-GEN LOC-1))
(define LOC-3 (cons CITY-ZUR LOC-2))

#;
(define (loc-templ loc)
  (cond [(empty? loc) ..]
        [(cons?  loc) (... (city-name (first loc)) (city-county (first loc))
                           (city-pop (first loc)) (loc-templ (rest loc) ...)) ]))
;c
;city-max: LoC -> City
;finds the largest city in a ListofCity

(check-expect (city-max LOC-1) CITY-BOS)
(check-expect (city-max LOC-2) CITY-BOS)
(check-expect (city-max LOC-3) CITY-BOS)
(check-expect (city-max (cons CITY-ZUR (cons CITY-GEN LOC-0))) CITY-ZUR)
(check-expect (city-max LOC-0) CITY-0)

(define (city-max loc)
  (cond [(empty? loc) CITY-0]
        [(cons?  loc) (city-max-2 (first loc) (city-max (rest loc)))]))

;d
; city-max would satisfy the following check-expect
(check-expect (city-max (list CITY-LUC CITY-GEN CITY-ZUR)) CITY-GEN)
; city-mx nor city-max-2 does not have to be changed
; because city-max2 is designed so that if
; city1 and city2 are euqal, city1 would be returned. since the first requirement of
; (< (city-pop city1) (city-pop city2)) is not fulfilled. 


