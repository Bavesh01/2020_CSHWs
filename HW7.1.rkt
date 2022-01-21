;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define-struct country [name capital population landlocked neighbors])
 
; A Country is a (make-country String String Nat Boolean [List-of String])
; repr. data on a country, where
; - name is the country's name
; - capital is the name of the country's capital
; - population is the (approx.) population of the country IN MILLIONS
; - landlocked is true iff the country does not have a coastline
; - neighbors is a list of names of all countries with which the country has a common border
(define CO-0 (make-country "United States" "Washington"   330 #f
                           (list "Canada" "Mexico")))
(define CO-1 (make-country "Japan"         "Tokyo"        130 #f '()))
(define CO-2 (make-country "Switzerland"   "Bern"           9 #t
                           (list "Germany" "Austria" "Lichtenstein" "Italy" "France")))
(define CO-3 (make-country "France"        "Paris"         70 #f
                           (list "Belgium" "Germany" "Switzerland" "Italy" "Spain")))
(define CO-4 (make-country "Argentina"     "Buenos Aires"  50 #f
                           (list "Paraguay" "Brazil" "Uruguay" "Chile" "Bolivia")))
 
(define (country-templ c)
  (... (country-name       c) ... (country-capital    c) ... (country-population c)
       ... (country-landlocked c) ... (country-neighbors  c) ...))
 
(define-struct city [name country population megacity languages])
 
; A City is a (make-city String String Nat Boolean [List-of String])
; repr. data on a city, where
; - name is the name of the city
; - country is the name of the country to which the city belongs
; - population is the (approx.) population of the city IN THOUSANDS
; - megacity is true iff the city has a population of 1 million people or more
; - languages is a list of names of /natural/ languages (not ISL) officially spoken in that city.
(define CI-0 (make-city "Washington" "United States" 5000 #t (list "English" "Spanish")))
(define CI-1 (make-city "Bern"       "Switzerland"    500 #f (list "German" "French" "Rhaetian")))
(define CI-2 (make-city "Boston"     "United States"  700 #f (list "English" "Spanish" "Chinese")))
(define CI-3 (make-city "Nairobi"    "Kenya"         5000 #t (list "Swahili" "English")))
 
(define (city-templ c)
  (... (city-name     c) ... (city-country   c) ... (city-population c)
       ... (city-megacity c) ... (city-languages c) ...))
 
; A Location is one of
; - Country
; - City
; repr. a geographic location.
(define LOC-0 CO-0)
(define LOC-1 CI-0)
#;
(define (location-templ l)
  (cond [(country? l) (... (country-templ l) ...)]
        [(city?    l) (... (city-templ    l) ...)]))
 
; An Atlas is a [List-of Location]
; A list of Country-ies and City-ies.
; (We are not providing a template, since this data is of the form, [List-of ...].)
(define A-0 (list CO-0 CO-1 CO-2 CO-3 CO-4 CI-0 CI-1 CI-2 CI-3))
;----------------------------------------------------------------------------------------------

;Exercise 1
(define A-1 (list CO-0 CO-1 CO-2 CO-3 CO-4 CI-0 CI-1 CI-2))
(define A-2 (list CO-1 CO-2 CO-3 CO-4 CI-0 CI-1))
(define A-3 (list CO-2 CI-0 CI-1 CI-2 CI-3))
(define A-4 (list CO-0 CO-1 CO-4 CI-0 CI-3))
(define A-5 (list CO-0 CO-1))
(define A-6 (list CI-0))

;Exercise 2
;countries: Atlas -> [List-of Countries]
;returns the Country-ies in an Atlas
(check-expect (countries A-0) (list CO-0 CO-1 CO-2 CO-3 CO-4))
(check-expect (countries A-1) (list CO-0 CO-1 CO-2 CO-3 CO-4))
(check-expect (countries A-2) (list CO-1 CO-2 CO-3 CO-4))

(define (countries a)
  (filter country? a))

;cities:  Atlas -> [List-of Cities]
;returns all the City-ies in an Atlas
(check-expect (cities A-0) (list CI-0 CI-1 CI-2 CI-3))
(check-expect (cities A-4) (list CI-0 CI-3))
(check-expect (cities A-2) (list CI-0 CI-1))

(define (cities a)
  (filter city? a))

;Exercise 3
;cities-us: Atlas -> [List-of Cities]
;returns all the City-ies in the US from an Atlas
(check-expect (cities-us A-0) (list CI-0 CI-2))
(check-expect (cities-us A-1) (list CI-0 CI-2))
(check-expect (cities-us A-4) (list CI-0))

(define (cities-us a)
  (filter us? (cities a)))

;us?: City -> Boolean
;determines if a City is from the US
(check-expect (us? CI-1) #false)
(check-expect (us? CI-0) #true)

(define (us? c)
  (string=? (city-country c) "United States"))

;Exercise 4
;neighbors-germany: Atlas -> [List-of Country]
;returns Country-ies that border Germany
(check-expect (neighbors-germany A-0) (list CO-2 CO-3))
(check-expect (neighbors-germany A-3) (list CO-2))
(check-expect (neighbors-germany A-4) (list))

(define (neighbors-germany a)
  (filter borders-with-germany? (countries a)))

;borders-with-germany?: Country -> Boolean
;Returns true if a country borders Germany.
(check-expect (borders-with-germany? CO-0) #f)
(check-expect (borders-with-germany? CO-2) #t)
(check-expect (borders-with-germany? CO-1) #f)
                
(define (borders-with-germany? c)
  (ormap germany? (country-neighbors c)))

;germany?: String -> Boolean
;Checks if string is equal to "Germany".
(check-expect (germany? "Germany") #t)
(check-expect (germany? "Canada")  #f)

(define (germany? count)
  (string=? count "Germany"))

;Exercise 5

(define CI-0.0 (make-city "Washington" "United States" 5000 #t (list)))
(define CI-1.0 (make-city "Bern"       "Switzerland"    500 #f (list)))
(define CI-2.0 (make-city "Boston"     "United States"  700 #f (list)))
(define CI-3.0 (make-city "Nairobi"    "Kenya"         5000 #t (list)))

;atlas-w/o-lang-data: Atlas -> Atlas
;takes away the language data on each City
(check-expect (atlas-w/o-lang-data A-0)
              (list CO-0 CO-1 CO-2 CO-3 CO-4 CI-0.0 CI-1.0 CI-2.0 CI-3.0))
(check-expect (atlas-w/o-lang-data A-1)
              (list CO-0 CO-1 CO-2 CO-3 CO-4 CI-0.0 CI-1.0 CI-2.0))
(check-expect (atlas-w/o-lang-data A-5) A-5)            

(define (atlas-w/o-lang-data a)
  (map rem-lang a))

;rem-lang: Location -> Location
;removes the languages of a city 
(check-expect (rem-lang CI-0) CI-0.0)
(check-expect (rem-lang CI-3) CI-3.0)

(define (rem-lang c)
  (if (country? c)
      c
      (make-city (city-name c)
                 (city-country c)
                 (city-population c)
                 (city-megacity c) '() )))

;Exercise 6
;countries-population: Atlas -> Nat
;returns total number of people "represented" in the countries of an Atlas.
(check-expect (countries-population A-0) 589)
(check-expect (countries-population A-6) 0)

(define (countries-population a)
  (foldr + 0 (map country-population (countries a))))

;Exercise 7
;check-population: Atlas -> Boolean
;checks if the combined City population exceeds the combined Country population
(check-expect (check-population A-0) #t)
(check-error (check-population A-3) (error-message A-3))
(check-error (check-population A-6) (error-message A-6))
  
(define (check-population a)
  (if (<= (cities-population a) (* (countries-population a) 1000))
      #t
      (error (error-message a))))
       

;cities-population: Atlas -> Nat
;returns total number of people "represented" in the cities of an Atlas.
(check-expect (cities-population A-3) 11200)
(define (cities-population a)
  (foldr + 0 (map city-population (cities a))))

;error-message: Atlas -> String
;Returns the error message needed to be displayed if the Atlas failes the test in check-population.
(check-expect (error-message A-3)
              (string-append "check-population: error: atlas has 11200 thousand people in all cities"
                             " but only 9 million people in all countries"))
(check-expect (error-message A-6)
              (string-append "check-population: error: atlas has 5000 thousand people in all cities"
                             " but only 0 million people in all countries"))

(define (error-message a)
  (string-append "check-population: error: atlas has "
                 (number->string (cities-population a))
                 " thousand people in all cities but only "
                 (number->string (countries-population a))
                 " million people in all countries"))

;Exercise 8

; I filled out the halftime class survey - Bavesh
; I filled out the halftime class survey - Michael
; I filled out the halftime class survey - Rosan