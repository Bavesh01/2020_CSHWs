;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW6.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;exercise 1

; A Bit is one of
; - "0"
; - "1"
; repr. a bit.
(define B-0 "0")
(define B-1 "1")
#;
(define (bit-templ b)
  (cond [(string=? b "0") ...]
        [(string=? b "1") ...]))
 
; A BitString is a String over Bits
; repr. a bitvector (which is a technical term).
(define BS-0 "")
(define BS-1 "001")
(define BS-2 "101")
#;
(define (bitstring-templ bs)
  (... bs ...))

(define LOB-1 (list #f #t #f #t #f))
(define LOB-4 (list #f #f #f #t #f))
(define LOB-2 (list))
(define LOB-3 (list #f))

;a
;bl2bs: [List-of Boolean] -> BitString
;converts ListOfBoolean to corresponding BitString

(check-expect (bl2bs LOB-1) "01010")
(check-expect (bl2bs LOB-4) "00010")
(check-expect (bl2bs LOB-2) "")
(check-expect (bl2bs LOB-3) "0")

(define (bl2bs lob)
  (foldr string-append "" (map correspond lob)))

;correspond: Boolean -> Bit
;converts Boolean to corresponding Bit
(check-expect (correspond #t) "1")
(check-expect (correspond #f) "0")

(define (correspond b)
  (if b "1" "0"))
       
;b
;bs2bl: BitString -> [List-of Boolean]
;converts BitString to ListOfBoolean
(check-expect (bs2bl BS-0) '())
(check-expect (bs2bl BS-1) (list #f #f #t))
(check-expect (bs2bl BS-2) (list #t #f #t))

(define (bs2bl bs)
  (map anti-correspond (explode bs)))

;anti-correspond: Bit -> Boolean
;converts Bit to corresponding Boolean
(check-expect (anti-correspond B-0) #f)
(check-expect (anti-correspond B-1) #t)

(define (anti-correspond bit)
  (string=? bit "1"))

;c
; list=? : (X) [List-of X] [List-of X] [X X -> Boolean] -> Boolean 
; return #t iff the two lists are equal
(check-expect (list=? '()         '()      string=?)  #t)
(check-expect (list=? '(1 2 3)    '(1 2 3) =)         #t)
(check-expect (list=? '(#t #f #t) '(#f #t) boolean=?) #f)
(define (list=? l1 l2 is=?)
  (cond [(empty? l1) (empty? l2)]
        [(cons?  l1) (and (cons? l2)
                          (is=? (first l1) (first l2))
                          (list=? (rest l1) (rest l2) is=?))]))


;d
;test-double-conv: [List-of Boolean] -> Boolean 
;checks if ListOfBooleans is same after conversion to BitString and back
(check-expect (test-double-conv LOB-2) #t)
(check-expect (test-double-conv LOB-1) #t)
(check-expect (test-double-conv LOB-4) #t)
(check-expect (test-double-conv LOB-3) #t)

(define (test-double-conv lob)
  (list=? (bs2bl(bl2bs lob)) lob boolean=?))
  
;Exercise 2

(define LOS-0 '())
(define LOS-1  (list "hello" "hello" "world"))
(define LOS-2  (list "hello"))
(define LOS-3  (list "hello" "hello" "world" "world"))
(define LOS-4  (list "I" "really" "don't" "really" "know"))

;a 
;word-counts: [List-of String] -> [List-of Numbers]
;Counts the frequency of every word in a text. 
(check-expect (word-counts LOS-0) (list))
(check-expect (word-counts LOS-1) (list 2 2 1))
(check-expect (word-counts LOS-2) (list 1))
(check-expect (word-counts LOS-3) (list 2 2 2 2))
(check-expect (word-counts LOS-4) (list 1 2 1 2 1))

(define (word-counts los )
  (cond [(empty? los) '()]
        [(cons? los)  (word-counter los los)]))

;word-counter: [List-of String] [List-of String] -> [List-of Numbers]
;Determines the frequency of every word in a list as a list of numbers.
(check-expect (word-counter LOS-1 LOS-1) (list 2 2 1))
(check-expect (word-counter LOS-2 LOS-2) (list 1))
(check-expect (word-counter LOS-4 LOS-4) (list 1 2 1 2 1))

(define (word-counter los2 los1)
  (cond [(empty? los2) '()]
        [(zero? (counter (first los2) los1)) '()]
        [(cons? los2) (cons (counter     (first los2) los1)
                            (word-counter (rest los2) los1))]))


;counter: String [List-of String] -> Number
;Counts the number of times a single string has been repeated in a list of strings.
(check-expect (counter "hello"  LOS-1) 2)
(check-expect (counter "really" LOS-4) 2)
(check-expect (counter "I"      LOS-4) 1)
(check-expect (counter "Rosan"  LOS-4) 0)

(define (counter str los2)
  (cond [(empty? los2) 0]
        [(cons? los2) (if (string=? str (first los2))
                          (add1 (counter str (rest los2)))
                          (counter str (rest los2)))]))

;b
;char-counts: String -> [List-of Numbers]
;Counts the frequency of every character in a word.
(check-expect (char-counts "really") (list 1 1 1 2 2 1))
(check-expect (char-counts "") (list))

(define (char-counts str)
  (word-counter (explode str) (explode str)))

;c
;counts:(X) [List-of X] [X X -> Boolean] -> [List-of Numbers]
;Returns the list of frequencies of each datatype that occurs in a list of that datatype.
(check-expect (counts (list 1 2 3 2 1) =)
              (list 2 2 1 2 2 ))
(check-expect (counts LOS-4 string=?) (list 1 2 1 2 1))
(check-expect (counts (list) =) (list))

(define (counts lox is=?)
  (cond [(empty? lox) '()]
        [(cons? lox)  (x-counter lox lox is=?)]))

;x-counter: (X) [List-of X] [List-of X] [X X -> Boolean]  -> [List-of Numbers]
;Returns the frequency of every element in a list as a list of numbers.
(check-expect (x-counter (list 1 2 3 2 1) (list 1 2 3 2 1) =)
              (list 2 2 1 2 2 ))
(check-expect (x-counter LOS-4 LOS-4 string=?) (list 1 2 1 2 1))

(define (x-counter lox2 lox1 is=?)
  (cond [(empty? lox2) '()]
        [(zero? (x-counter-single (first lox2) lox1 is=?)) '()]
        [(cons? lox2) (cons (x-counter-single (first lox2) lox1 is=?)
                            (x-counter (rest lox2) lox1 is=?))]))

;x-counter-single: (X) X [List-of X] [X X -> Boolean] -> Number
;Counts the number of times a single element has been repeated in a list.
(check-expect (x-counter-single 1 (list 1 2 3 2 1) =) 2)
(check-expect (x-counter-single "really" LOS-4    string=?) 2)
(check-expect (x-counter-single "l" (explode "really") string=?) 2)
(check-expect (x-counter-single "b" (explode "really") string=?) 0)

(define (x-counter-single x lox is=?)
  (cond [(empty? lox) 0]
        [(cons? lox) (if (is=? x (first lox))
                         (add1 (x-counter-single x (rest lox) is=?))
                         (x-counter-single x (rest lox) is=?))]))

