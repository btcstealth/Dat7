#lang racket

#|
Functions for building the html reprentation
|#
(define tagCreator(lambda(tag)
                    (lambda(attributes . contents)
                      (helpFunc contents tag))))

(define helpFunc(lambda(cont tag)
                  (cond ((null? cont) "")
                            ((string? cont) (string-append "<"tag">" cont "</"tag">"))
                            ((list? cont) (helpFunc ((car cont) (list-ref cont 1) "</"tag">")))
                            ;((list? cont) (helpFunc (string-append "<"tag">" (car cont) (list-ref cont 1) "</"tag">")))
                            ;((list? cont) (string-append "<"tag">" (car cont) (list-ref cont 1) "</"tag">"))
                            
                            ((procedure? cont) (string-append cont))
                            (else (string-append "<"tag">" (car cont) (list-ref cont 1) "</"tag">"))
                            ;((string? (car cont)) (string-append (car cont)) (helpFunc (cdr cont) tag))
                  )))

;; (string-append (car lst) (cdr lst)) 

(define html(tagCreator "html"))
(define head(tagCreator "head"))
(define body(tagCreator "body"))
(define table(tagCreator "table"))
(define span(tagCreator "span"))
(define h1(tagCreator "h1"))


#|
(html "attributes"
      (head "attributes" "test" "")
      (body "attributes" "" ""))
|#

;;(html "attributes" (list "test1" "test2") "")


#|
Functions for handling the time for each
|#
(define createTime (lambda(year month day hour minute)
                     (list (checkYear year) (checkMonth month) (checkDay day) (checkHour hour) (checkMinute minute))))

(define checkYear (lambda (year)
                    (if (> year 0)
                        year
                        (error "the format of year is unacceptable, year has be bigger than 0"))))

(define checkMonth (lambda (month)
                     (if (and (> month 0) (< month 13))
                         month
                         (error "the format of month is unacceptable"))))

(define checkDay (lambda (day)
                   (if (and (> day 0) (< day 32))
                       day
                       (error "the format of day is unacceptable"))))

(define checkHour (lambda (hour)
                    (if (and (> hour -1) (< hour 24))
                        hour
                        (error "the format of hour is unacceptable"))))

(define checkMinute (lambda (minute)
                      (if (and (> minute -1) (< minute 61))
                          minute
                          (error "the format of minute is unacceptable"))))

#|
Function for calculating the time in minutes
|#
(define calcTimeSeconds (lambda (time)
                   (+ (calcYear (car time)) (calcMonth (list-ref time 1)) (calcDay (list-ref time 2)) (calcHour (list-ref time 3)) (calcMinute (list-ref time 4)))))

(define calcYear (lambda (year)
                   (* year 12 31 24 60 60)))

(define calcMonth (lambda (month)
                    (* month 31 24 60 60)))

(define calcDay (lambda (day)
                  (* day 24 60 60)))

(define calcHour (lambda (hour)
                   (* hour 60 60)))

(define calcMinute (lambda (minute)
                   (* minute 60)))

;; do something to time to calc time since date
(define isT1BiggerThanT2( lambda(time1 time2)
                           (if (> (calcTimeSeconds time1) (calcTimeSeconds time2))
                               't
                               'f
                               )))

;; from-time < (car apt) < to-time or from-time < (car (cdr apt)) < to-time

(define appointmentsInInterval?( lambda(apt from-time to-time)
                                 (if (or
                                      (and (< (calcTimeSeconds from-time) (calcTimeSeconds (car apt)))
                                           (> (calcTimeSeconds to-time) (calcTimeSeconds (car apt))))
                                      (and (< (calcTimeSeconds from-time) (calcTimeSeconds (car (cdr apt))))
                                           (> (calcTimeSeconds to-time) (calcTimeSeconds (car (cdr apt))))))
                                     #t
                                     #f
                                     )))

(define present-appointments-in-interval (lambda(cal from-time to-time res)
                                           (if (null? cal)
                                               res
                                               (if (appointmentsInInterval? (car (parseCalendar cal '())) from-time to-time)
                                                   (present-appointments-in-interval (cdr (parseCalendar cal '())) from-time to-time (cons (car (parseCalendar cal '())) res))
                                                   (present-appointments-in-interval (cdr (parseCalendar cal '())) from-time to-time res)
                                                   )))) 

#|
Alternative:
10^8 year
10^6 month
10^4 day
10^2 hour
10^1 min
|#

                   
#|
Functions for getting different elements from the appointment time list
|#
(define getYear first)
(define getMonth second)
(define getDay third)
(define getHour fourth)
(define getMinute fifth)


#|
Internal calender representation: The root is a calender which is a list of appointments
|#
;; add logic here to make sure that startTime and endTime has a specific max interval in between
(define createAppointment( lambda(startTime endTime content)
                            (if (< (calcTimeSeconds startTime) (calcTimeSeconds endTime))
                                (list startTime endTime content)
                                (error "startTime has to be before endTime")))) ;;has to be changed to it completely stops

(define createCalender( lambda(apt1 . aptn) (apply list "calendar" apt1 aptn)))

#|
Functions for parsing through the calendar
|#
(define parseCalendar( lambda(cal res)
                        (if (empty? cal)
                            res
                            (cond ((checkIfCalendar? cal) (parseCalendar (cdr cal) res))
                                  ((checkIfCalendar? (car cal)) (append (parseCalendar (cdr cal) res) (parseCalendar (car cal) '()))) ; (flattenCal (car cal) cal res))
                                  ((checkIfAppointment? (car cal)) (parseCalendar (cdr cal) (cons (car cal) res)))
                                  (else (parseCalendar (cdr cal) res)) ;; for some reason run into else
                                  ))))

#|
(define flattenCal( lambda(cal calMem res)
                     (if (empty? cal)
                         (parseCalendar (cdr calMem) res)
                         (cond ((checkIfCalendar? cal) (parseCalendar (cdr cal) res))
                               ((checkIfCalendar? (car cal)) (flattenCal (cdr cal) calMem res))
                               ((checkIfAppointment? (car cal)) (flattenCal (cdr cal) calMem (cons (car cal) res)))
                               ))))
|#


(define checkIfCalendar?( lambda(cal)
                          (if (list? cal)
                                (if (string? (car cal))
                                      (eqv? (car cal) "calendar")
                                      #f
                                      )
                                #f)))

;;this needs fixing
(define checkIfAppointment?( lambda(cal)
                             (if (list? cal)
                                 (string? (last cal))
                                 #f)))


#|
Required functions
|#

;;add appointment
(define addAppointmentToCalendar( lambda(cal apt)
                                   (append (flatten-calendar cal) (list apt))))

;;add calendar
(define addCalendarToCalendar( lambda (cal addCal)
                                (addAppointmentToCalendar cal addCal)))

;;delete appointment
(define deleteAppointmentFromCalendar( lambda(cal apt)
                                        (remv apt (flatten-calendar cal)))) ;; doesn't work for some reason

;;delete calendar
(define deleteCalendarFromCalendar( lambda(cal delCal)
                                     (deleteAppointmentFromCalendar cal delCal)))
                         

(define present-calendar-html( lambda(cal from-time to-time)
                                "not implemented")) ;;not made 


(define find-appointments (lambda(cal pred)
                            (filter pred (parseCalendar cal '()))
                            ))

(define find-first-appointment (lambda(cal pred)
                                 (filter pred (parseCalendar cal '())) ;; add logic to only get earliest appointment
                                 ))

(define find-last-appointment (lambda(cal pred)
                                 (filter pred (parseCalendar cal '())) ;; add logic to only get latest appointment
                                 ))

(define flatten-calendar (lambda(cal)
                           (cons "calendar" (parseCalendar cal '())))) 

(define appointments-overlap? (lambda(ap1 ap2)
                                "not implemented"))

(define calendars-overlap? (lambda(cal1 cal2)
                             "not implemented"))
;;compare: apt1 endTime < apt2 startTime && apt2 endTime < apt1 startTime 



#|
Ignore for now
|#
(define findAttributeInLst(lambda(lst res)
                            (if (empty? lst)
                                res
                                [cond ((symbol? (car lst)) (findAttributeInLst (cdr lst) (cons (car lst) res)))
                                      ((list? (car lst)) (findAttributeInLst (car lst) '()) (findAttributeInLst (cdr lst) res)) ; do something here
                                      (else (findAttributeInLst (cdr lst) res))
                                ]
                                )))

(define findContentInLst(lambda(lst res)
                          (if (empty? lst)
                                res
                                [if (string? (car lst))
                                    [findContentInLst (cdr lst) (cons (car lst) res)]
                                    [findContentInLst (cdr lst) res]
                                ]
                                )))

;(findAttributeInLst (list (list 's 'b 'd) 'd 3 'g) '())

#|
Testing functionality section
|#
;;husk at tilfoeje funktioner til at tjekke om calendar er legal...
(define cal1 (createCalender
              (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content1")
               (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content2") 
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass1")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass2"))
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass3")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass4"))
              (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 56) "pass5")
              ;(createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content")
              ;(createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content") 
              (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content3") 
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass6")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass7")
               (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content4") 
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass8")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass9")))
              (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "pass10")))

;cal1
;(appointmentsInInterval? (car (cdr (parseCalendar cal1 '()))) (createTime 2005 11 24 10 55) (createTime 2005 11 24 20 54))
;(present-appointments-in-interval cal1 (createTime 2005 11 24 10 55) (createTime 2005 11 24 20 54) '())
"-----------------------------------------"
(flatten-calendar cal1) 
"-----------------------------------------"
;(addAppointmentToCalendar cal1 (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content22"))
#|
(addCalendarToCalendar cal1 (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content") 
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass2")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass1")))
|#
;(deleteAppointmentFromCalendar cal1 (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content"))
;(createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 56) "pass3")
;"-----------------------------------------"
