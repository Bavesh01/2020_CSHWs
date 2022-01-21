;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Exercise 1
;a)
(define-struct bug [location temperature])
; A Bug is a (make-bug String Number)
; A bug in the Line world, where
; -location is the coordinate number where the bug is
; -temperature is the body temperature of the bug in Farenheit
(define BUG1 (make-bug 20 95))
(define BUG2 (make-bug 20 86))
(define BUG3 (make-bug 40 76))
#;
(define (bug-templ b)
  ( ... (bug-location    b) ...
        (bug-temperature b) ...))

;b)
(define-struct bird [name location sing?])
; A Bird is a (make-bug String Number Boolean)
; A bird in the Line world, where
; -name is the name of the bird
; -location is the coordinate number where the bird is
; -sing? is whether the bird can sing
(define BIRD1 (make-bird "peacock" 23 #t))
(define BIRD2 (make-bird "chicken" 40 #f))
(define BIRD3 (make-bird "eagle"   52 #f))
(define BIRD4 (make-bird "siren"   00 #t))
(define BIRD5 (make-bird "cuckoo"  98 #t))

#;
(define (bird-templ b)
  (... (bird-location    b) ...
       (bird-temperature b) ...
       (bird-sing?       b) ...))

;c)
; An Animal is one of:
; -Bird
; -Bug
; -"Nessie"
; Represents the kind of animal Animal is, or #t if it's Nessie
(define ANIMAL-1 BUG1)
(define ANIMAL-2 BIRD1)
(define ANIMAL-3 BUG2)
(define ANIMAL-4 BIRD2)
(define ANIMAL-5 BUG3)
(define ANIMAL-N "Nessie")
(define ANIMAL-6 BIRD3)
(define ANIMAL-7 BIRD4)
(define ANIMAL-8 BIRD5)
#;
(define (animal-templ a)
  ( ... (if (animal? a)
            (cond [(bug?     a) ... (bug-templ  a) ...]
                  [(bird?    a) ... (bird-templ a) ...]
                  [(string=? a "Nessie")      ...     ]) ...)))

; animal? Any -> Boolean
; returns true exactly if the given data is an Animal. 
(check-expect (animal? ANIMAL-1) #t)
(check-expect (animal? ANIMAL-2) #t)
(check-expect (animal? ANIMAL-N) #t)
(check-expect (animal? "animal") #f)
(define (animal? a)
  (or (bug?     a)
      (bird?    a)
      (string=? a "Nessie")))

;d)
(define-struct TwoAnimals [a1 a2])
; TwoAnimals is a (make-TwoAnimals Animal Animal)
; Represents a group of two animals.
(define TA-1 (make-TwoAnimals ANIMAL-1 ANIMAL-2))
(define TA-2 (make-TwoAnimals ANIMAL-N ANIMAL-1))
(define TA-3 (make-TwoAnimals ANIMAL-4 ANIMAL-3))
(define TA-4 (make-TwoAnimals ANIMAL-5 ANIMAL-1))
(define TA-5 (make-TwoAnimals ANIMAL-1 ANIMAL-3))

#;
(define (twoanimals-templ ta)
  ( ... (animal-templ (TwoAnimals-a1 ta)) ...
        (animal-templ (TwoAnimals-a2 ta)) ...))

;e)
; danger? : TwoAnimals -> Boolean
; Returns true if  TwoAnimals contains a bug and a bird AND
; the two are less than 10 pixels apart from each other
(check-expect (danger? TA-1) #t)
(check-expect (danger? TA-2) #f)
(check-expect (danger? TA-3) #f)
(define (danger? ta)
  (or (and (and (bird? (TwoAnimals-a1 ta))
                (bug? (TwoAnimals-a2 ta)))
           (< (abs (-  (bird-location (TwoAnimals-a1 ta))
                       (bug-location  (TwoAnimals-a2 ta)))) 10))
      (and (and (bug?  (TwoAnimals-a1 ta))
                (bird? (TwoAnimals-a2 ta)))
           (< (abs (-  (bug-location  (TwoAnimals-a1 ta))
                       (bird-location (TwoAnimals-a2 ta)))) 10))))
;more tests to cover all combinations

;f)
; metamorph : TwoAnimals -> Aorta
; Returns Nessie if two bugs follow the specifications,
; returns the same TwoAnimals otherwise
(check-expect (metamorph TA-3) AORTA-1)
(check-expect (metamorph TA-4) AORTA-2)
(check-expect (metamorph TA-5) AORTA-3)
(define (metamorph ta)
  (if (and (bug? (TwoAnimals-a1 ta))
           (bug? (TwoAnimals-a2 ta))
           (> (bug-temperature (TwoAnimals-a1 ta)) 80)
           (> (bug-temperature (TwoAnimals-a1 ta)) 80)
           (= (bug-location (TwoAnimals-a1 ta))
              (bug-location (TwoAnimals-a2 ta))))
      AORTA-3
      ta))

; Aorta is one of:
; -TwoAnimals
; -Animal
; Represents the possible results of the metamorph function
(define AORTA-1 TA-3)
(define AORTA-2 TA-4)
(define AORTA-3 ANIMAL-N)
#;
(define (aorta-templ a)
  ( ... (cond [(TwoAnimals? a) ... (twoanimals-templ a) ... ]
              [(animal?     a) ... (animal-templ     a) ... ]) ...))

; Exercise 2

; 1. Improper design recipe: Line 2 move-CS-Purpose is not descriptive,
; its a reiteration of signature
; 2. Improper design recipe: Line 1 function name 'move-CS' should be lowercase
; 3. Error Line 6: Should use recognizer predicates that evaluates both cases for twocars? and wreck?
; to have increased reliability and adherence to defined templates.

; move-cs : CS -> CS
; updates the simulation 
#;
(check-expect (move-cs CS-1) (move-twocars CS-1))
#;
(check-expect (move-cs CS-3) (move-wreck   CS-3))
#;
(define (move-cs cs)
  (cond [(twocars? cs) (move-twocars cs)]
        [(wreck? cs) (move-wreck cs)]))

; 4. Style violation: Line 15 is wronglly indented
; 5. Style violation: Line 16 is over 102 characters
; 6. Style violation: Line 16- the usage of the value 0.1 seems vague and inreliable.
; should be defined as a constant.
; 7. Style violation: Line 10- Improper capitalization of  data type Twocars, should be TwoCars
; 8. Improper design recipe: There are no check-expects made for move-twocars.
; 9. Improper design recipe: Line 11- The purpose statement is vague,
; should consider the movement of both cars.
; 10. Improper design recipe: Line 10- The signature is wrong.
; The outpupt is a union of TwoCars and a wreck, i.e. a CS.


; move-twocars : TwoCars -> CS
; moves both the cars in a TwoCars during 1 clock tick,
; or moves the wreck if the TwoCars have collided
(define ACCEL 0.1)
#;
(check-expect (move-twocars TWOCARS-1)
              (make-twocars 3 2.1 596 4.1))
#;
(check-expect (move-twocars TWOCARS-2)
              WRECK-1)
#;
(define (move-twocars tc)
  (if (collided? tc)
      (make-wreck (/ (+ (twocars-x1 tc) (twocars-x2 tc)) 2)
                  (- (twocars-v1 tc) (twocars-v2 tc)))
      (make-twocars (+ (twocars-x1 tc) (twocars-v1 tc))
                    (+ (twocars-v1 tc) ACCEL)
                    (- (twocars-x2 tc) (twocars-v2 tc))
                    (+ (twocars-v2 tc) ACCEL))))

; Exercise 3

;a)
(define-struct zoo [animal others])
; A Zoo is a (make-zoo Animal zoo)
; A representation of arbitrary number of animals, where
; -animal is an animal in the zoo
; -zoo is a group of animals
; -or it's the value 0.
(define ZOO-0 0)
(define ZOO-1 (make-zoo ANIMAL-1 ZOO-0))
(define ZOO-2 (make-zoo ANIMAL-2 ZOO-1))
(define ZOO-3 (make-zoo ANIMAL-3 ZOO-2))
(define ZOO-4 (make-zoo ANIMAL-4 ZOO-3))
(define ZOO-5 (make-zoo ANIMAL-5 ZOO-4))
(define ZOO-6 (make-zoo ANIMAL-6 ZOO-5))
(define ZOO-7 (make-zoo ANIMAL-7 ZOO-6))
(define ZOO-8 (make-zoo ANIMAL-8 ZOO-7))

#;
(define (zoo-templ z)
  (... (animal-temp (zoo-animal z))
       (zoo-temp z))...)

;b)
; the-birds : Zoo -> String
; Lists the names of birds in the zoo (in proper semantics)
(check-expect (the-birds ZOO-0) "")
(check-expect (the-birds ZOO-4) "chicken, peacock")
(check-expect (the-birds ZOO-8) "cuckoo, siren, eagle, chicken, peacock")
(define (the-birds z)
  (if (string=? (list-birds z) "")
      ""
      (substring (list-birds z)
                 2
                 (string-length (list-birds z)))))

; list-birds: Zoo -> String
; Lists the names as a succession of ", birdname" format.
(check-expect (list-birds ZOO-4) ", chicken, peacock")
(check-expect (list-birds ZOO-8) ", cuckoo, siren, eagle, chicken, peacock")
(define (list-birds z)
  (cond [(number? z) ""]
        [(zoo? z) (string-append (if (bird? (zoo-animal z)) ", " "")
                                 (get-birdname z) 
                                 (list-birds (zoo-others z)))]))


; get-birdname: Zoo -> String
; get the name of the birds
(check-expect (get-birdname ZOO-2) "peacock")
(check-expect (get-birdname ZOO-3) "")
(define (get-birdname z)
  (if (bird? (zoo-animal z))
      (bird-name (zoo-animal z))
      ""))