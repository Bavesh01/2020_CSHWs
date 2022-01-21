;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 1
(define SCENE (rectangle 1000 100 "solid" "black"))
(define SIZE  20)
(define COLOR "gold")

(require 2htdp/image)
(require 2htdp/universe)

; monitor-account: PosInteger -> PosInteger
; Displays the balance of a bank account
(define (monitor-account n)
  (big-bang n
    [to-draw draw-bar]                     ; PosInteger -> Image
    [on-tick calculate 1]                  ; PosInteger -> PosInteger
    [stop-when end-program? end-program])) ; PosInteger -> Boolean

; draw-bar: PosInteger -> Image
; Draws the current state i.e. the amount of balance left in the account.
(check-expect (draw-bar 1)  (overlay (rectangle 10  SIZE "solid" COLOR) SCENE))
(check-expect (draw-bar 15) (overlay (rectangle 150 SIZE "solid" COLOR) SCENE))
(define (draw-bar n)
  (overlay (rectangle (* 10 n) SIZE "solid" COLOR) SCENE))  

; calculate: PosInteger -> Number
; Returns the balance amount after the calculation of interest.
(check-expect (calculate 2) 9)
(check-expect (calculate 9) 4)
(define (calculate n)
  (cond [(odd? n)  (-(/(+ n 1) 2) 1)]
        [(even? n) (*(+ n 1) 3)]))
;else show error
   
; end-program?: Number -> Boolean
; Checks whether balance has reached 0
(check-expect (end-program? 0) #true)
(check-expect (end-program? 3) #false)
(define (end-program? n)
  (= n 0))

; end-program: Any -> Image
; Shows an ending screen
(check-expect (end-program "0") (overlay (text "You are broke!" SIZE COLOR) SCENE))
(check-expect (end-program 123) (overlay (text "You are broke!" SIZE COLOR) SCENE))
(define       (end-program n) (overlay (text "You are broke!" SIZE COLOR) SCENE))

; The customer should be very concerned because the
; wicked bank's policies would rob people's hard earned money and
; depletes the account balance to null no matter their initial deposit.
; For an initial balnce of the type 2^n-1, the balance only declines and never increases, Yikes!
; For an initial balnce of 49USD, the balance increases and decreases in random patterns.

; Exercise 2
; A TrafficLight is one of:
; - "red"
; - "yellow"
; - "green"
; Represents the colors of a traffic light
(define TL-RED    "red")
(define TL-YELLOW "yellow")
(define TL-GREEN  "green")
#;
(define (trafficlight-templ tl)
  (... (cond
         [(string=? tl TL-RED)    ...]
         [(string=? tl TL-YELLOW) ...]
         [(string=? tl TL-GREEN)  ...])))

; A Cycletime is one of:
; - 0
; - 1
; - 2
; - 3
; - 4
; - 5
; Represents the ticks of a visual of the traffic lights.
(define CT-0 0)
(define CT-1 1)
(define CT-2 2)
(define CT-3 3)
(define CT-4 4)
(define CT-5 5)
#;
(define (cycletime-templ ct)
  ( ... (cond
          [(= ct CT-0) ...]
          [(= ct CT-1) ...]
          [(= ct CT-2) ...]
          [(= ct CT-3) ...]
          [(= ct CT-4) ...]
          [(= ct CT-5) ...] ...)))

(define-struct ClockedLight [trafficlight cycletime])
; A ClockedLight is a (make-ClockedLight TrafficLight Cycletime)
; A Traffic light with a tick value
; - trafficlight is the color of the light
; - cycletime is the tick value it holds, to loop at certain time intervals.
(define CL-RED0    (make-ClockedLight TL-RED    0))
(define CL-RED1    (make-ClockedLight TL-RED    1))
(define CL-RED2    (make-ClockedLight TL-RED    2))
(define CL-GREEN3  (make-ClockedLight TL-GREEN  3))
(define CL-GREEN4  (make-ClockedLight TL-GREEN  4))
(define CL-YELLOW5 (make-ClockedLight TL-YELLOW 5))
(define CL-TEST    (make-ClockedLight TL-RED    4))
#;
(define (clcokedlight-templ cl)
  (... (trafficlight-templ  (ClockedLight-trafficlight cl)) ...
       (cycletime-templ     (ClockedLight-cycletime    cl)) ...)) 
                           
; loop-light : ClockedLight -> ClockedLight
; Visualizes a looping traffic light
(define (loop-light cl)
  (big-bang cl
    [to-draw draw-cl]     ; ClockedLight -> Image
    [on-tick next-cl 1])) ; ClockedLight -> ClockedLight

; draw-cl : ClockedLight -> Image
; Visualizes a traffic light
(define TL-RADIUS 50)
(check-expect (draw-cl CL-RED2)    (circle TL-RADIUS "solid" "red"))
(check-expect (draw-cl CL-YELLOW5) (circle TL-RADIUS "solid" "yellow"))
(check-expect (draw-cl CL-GREEN4)  (circle TL-RADIUS "solid" "green"))

#;
(define (draw-cl cl)
  (cond
    [(and (string=? (ClockedLight-trafficlight cl) TL-RED)
          (< (ClockedLight-cycletime cl) 3))
     (circle TL-RADIUS "solid" TL-RED)]
    [(and (string=? (ClockedLight-trafficlight cl) TL-GREEN)
          (< (ClockedLight-cycletime cl) 5))
     (circle TL-RADIUS "solid" TL-GREEN)]
    [(and (string=? (ClockedLight-trafficlight cl) TL-YELLOW)
          (= (ClockedLight-cycletime cl) 5))
     (circle TL-RADIUS "solid" TL-YELLOW)]))
#;
(define (draw-cl cl)
  (cond
    [(string=? (ClockedLight-trafficlight cl) TL-RED)
     (circle TL-RADIUS "solid" TL-RED)]
    [(string=? (ClockedLight-trafficlight cl) TL-GREEN)
     (circle TL-RADIUS "solid" TL-GREEN)]
    [(string=? (ClockedLight-trafficlight cl) TL-YELLOW)
     (circle TL-RADIUS "solid" TL-YELLOW)]))

(define (draw-cl cl)
  (cond
    [(< (ClockedLight-cycletime cl) 3)
     (circle TL-RADIUS "solid" TL-RED)]
    [(< (ClockedLight-cycletime cl) 5)
     (circle TL-RADIUS "solid" TL-GREEN)]
    [(= (ClockedLight-cycletime cl) 5)
     (circle TL-RADIUS "solid" TL-YELLOW)]))

; next-cl : ClockedLight -> ClockedLight
; Loops the light at given time intervals
(check-expect (next-cl CL-RED1)    CL-RED2)
(check-expect (next-cl CL-RED2)    CL-GREEN3)
(check-expect (next-cl CL-GREEN4)  CL-YELLOW5)
(check-expect (next-cl CL-YELLOW5) CL-RED0)
(define (next-cl cl)
  (cond
    [(< (loop-time cl) 3) (make-ClockedLight TL-RED    (loop-time cl))]
    [(< (loop-time cl) 5) (make-ClockedLight TL-GREEN  (loop-time cl))]
    [(= (loop-time cl) 5) (make-ClockedLight TL-YELLOW (loop-time cl))]))

; loop-time : ClockedLight -> Cycletime
; changes the cycletime of a ClockedLight in a loop from 0 to 5.
(check-expect (loop-time CL-RED2)    3)
(check-expect (loop-time CL-YELLOW5) 0)
(define (loop-time cl)
  (modulo (+ 1 (ClockedLight-cycletime cl)) 6))

; The result depends on the code used for the to-draw hhandler function.
; If the display of the Traffic Light is based on EITHER the ClockedLight-trafficLight
; or the ClockedLight-cycletime (range) value [code from 130 to 137 and 139 to 146],
; The display simply assumes "Green" 4
; as the initial display and (almost instantly) displays "Yellow" 5
; However, if we use 'and' function to allow only restricted values for the
; TrafficLight and the cycletime value (depending on the purpose) [code from 118 to 128],
; the code shows an error if the input is not of a recognized form.

; Exercise 3
(define-struct event [title starttime endtime location public creator])
; An Event is a (make-event String Fraction Fraction String Boolean String)
; The event of a manager.
; - title is the title of the event
; - starttime is when the event starts in decimal format
; - endtime is when the event ends in decimal format
; - location is the place where the event is being held
; - public is whether the event is announced on the website to public or not.
; - creator is the name of the event crator/manager.
(define EVENT1 (make-event "Meeting1" 13.75 14.5 "office" #false "Marcus"))
(define EVENT2 (make-event "Party"    18    20   "hotel"  #false "Lisa"))
(define EVENT3 (make-event "Expo"     10    14   "hall"   #true  "Roberts"))
#;
(define (event-templ e)
  ( ... (event-title e) ...     
        ... (event-starttime e) ...
        ... (event-endtime   e) ...   
        ... (event-location  e) ...  
        ... (event-public    e) ...
        ... (event-creator   e) ...)) 

; Functions created, and their signatures:
; make-event      : String Fraction Fraction String Boolean String -> Event
; event?          : Any -> Boolean
; event-title     : Event -> String
; event-starttime : Event -> Fraction
; event-endtime   : Event -> Fraction
; event-location  : Event -> String
; event-public    : Event -> Boolean
; event-creator   : Event -> String

; duration : Event -> Number
; Returns the duration of an event in minutes.
(check-expect (duration EVENT1) 45)
(check-expect (duration EVENT3) 240)
(check-error  (duration (make-event "Meeting1" 15 14 "office" #false "Marcus"))
              "event Meeting1: incorrect time specification: start > end")
(define (duration e)
  (cond [(>= (event-endtime e) (event-starttime e))
         (* 60 (- (event-endtime e) (event-starttime e)))]
        [else (error (string-append "event "
                                    (event-title e)
                                    ": incorrect time specification: start > end"))]))

; reschedule : Event Number -> Event
; Returns an event with a rescheduled starttime and endtime based on given inputs.
(check-expect (reschedule EVENT1 17)
              (make-event "Meeting1" 17 17.75 "office" #false "Marcus"))
(check-expect (reschedule EVENT3 12)
              (make-event "Expo"     12 16    "hall"   #true  "Roberts"))
(check-error  (reschedule EVENT2 22)
              "event Party: rescheduling extends event to midnight")
(define (reschedule e s)
  (cond [(< (+ s (/ (duration e) 60)) 24)
         (make-event (event-title e) s (+ (/ (duration e) 60) s)
                     (event-location e) (event-public e) (event-creator e))]
        [else (error (string-append  "event "
                                     (event-title e)
                                     ": rescheduling extends event to midnight"))]))
                     

; overlap? : Event Event -> Boolean
; Do the events overlap each other?
(check-expect (overlap? EVENT2 EVENT3) #f)
(check-expect (overlap? EVENT2 (reschedule EVENT3 14)) #t)
(check-expect (overlap? EVENT1 EVENT2) #f)
(check-expect (overlap? EVENT1 EVENT3) #t)
(define (overlap? e1 e2)
  (or (and (>= (event-endtime e1)   (event-starttime e2))
           (<  (event-starttime e1) (event-endtime e2)))
      (and (>= (event-endtime e2)   (event-starttime e1))
           (<  (event-starttime e2) (event-endtime e1))))) 