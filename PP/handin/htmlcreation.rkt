#lang racket

#|
Author:
Bjarke Thorn Carstens
bcarst09@student.aau.dk
studentnr. 20093648
|#


#|
Functions for building the html reprentation
|#

(define tagCreator(lambda(tag)
                    (lambda(attributes . contents)
                      (tagCreatorH attributes contents tag)
                      )))

(define tagCreatorH(lambda(attributes cont tag)
                     (if (empty? cont)
                         ""
                         (string-append "<" tag (findAttributes attributes) ">" (findContent cont) "</" tag ">")
                         )))


(define findContent( lambda(cont)
                      (if (empty? cont)
                          ""
                          (cond ((list? (car cont)) (string-append (car cont) (findContent (cdr cont))))
                                ((string? (car cont)) (string-append (car cont) (findContent (cdr cont))))
                                ((procedure? (car cont)) (string-append (car cont) (findContent (cdr cont))))
                                ))))

(define findAttributes( lambda(attributes)
                         (if (> (string-length attributes) 0)
                             (string-append " " attributes)
                             ""
                             )
                         ))                                                   

(define html(tagCreator "html"))
(define head(tagCreator "head"))
(define body(tagCreator "body"))
(define table(tagCreator "table"))
(define tr(tagCreator "tr"))
(define td(tagCreator "td"))
(define ul(tagCreator "ul"))
(define li(tagCreator "li"))
(define h1(tagCreator "h1"))


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
                         (error "the format of month is unacceptable, month has to be bigger than 0 and lower than 13"))))

(define checkDay (lambda (day)
                   (if (and (> day 0) (< day 32))
                       day
                       (error "the format of day is unacceptable, day has to be bigger than 0 and at most 31"))))

(define checkHour (lambda (hour)
                    (if (and (> hour -1) (< hour 24))
                        hour
                        (error "the format of hour is unacceptable, hour can be between 0-23"))))

(define checkMinute (lambda (minute)
                      (if (and (> minute -1) (< minute 60))
                          minute
                          (error "the format of minute is unacceptable, minute has to be between 0-59"))))

#|
Function for calculating the time in seconds
|#
(define calcTimeSeconds (lambda (time)
                   (+ (calcYear (getYear time)) (calcMonth (getMonth time)) (calcDay (getDay time)) (calcHour (getHour time)) (calcMinute (getMinute time)))))

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


;;not currently used
(define present-appointments-in-interval (lambda(cal from-time to-time res)
                                           (if (null? cal)
                                               res
                                               (if (appointmentsInInterval? (car (parseCalendar cal '())) from-time to-time)
                                                   (present-appointments-in-interval (cdr (parseCalendar cal '())) from-time to-time (cons (car (parseCalendar cal '())) res))
                                                   (present-appointments-in-interval (cdr (parseCalendar cal '())) from-time to-time res)
                                                   ))))


#|
Functions for getting different elements from the appoint
|#
(define getStartTime first)
(define getEndTime second)
(define getContent third)

                   
#|
Functions for getting different elements from the appointment time list
|#
(define getYear first)
(define getMonth second)
(define getDay third)
(define getHour fourth)
(define getMinute fifth)


#|
Internal calender representation: The root is a calender which is a list of appointments or calendars
|#
(define createAppointment( lambda(startTime endTime content)
                            (if (and (< (calcTimeSeconds startTime) (calcTimeSeconds endTime))
                                     (= (getYear startTime) (getYear endTime))
                                     (= (getMonth startTime) (getMonth endTime))
                                     (= (getDay startTime) (getDay endTime))) ;;make sure that startTime and endTime has to be located on the same day
                                (list startTime endTime content)
                                (error "startTime of appointment has to be before endTime, and have to be scheduled on the same year, month and day"))))

;;the calendar can take 
(define createCalender( lambda(apt1 . aptn)
                         (apply list "calendar" apt1 aptn)))

#|
Functions for parsing through the calendar
|#
(define parseCalendar( lambda(cal res)
                        (if (empty? cal)
                            res
                            (cond ((checkIfCalendar? cal) (parseCalendar (cdr cal) res))
                                  ((checkIfCalendar? (car cal)) (append (parseCalendar (cdr cal) res) (parseCalendar (car cal) '())))
                                  ((checkIfAppointment? (car cal)) (parseCalendar (cdr cal) (cons (car cal) res)))
                                  (else (parseCalendar (cdr cal) res))
                                  ))))

;;function for check if the parameter is a calendar
(define checkIfCalendar?( lambda(cal)
                          (if (list? cal)
                                (if (string? (car cal))
                                      (eqv? (car cal) "calendar")
                                      #f
                                      )
                                #f)))

;;naive function for check if the parameter is an appointment
(define checkIfAppointment?( lambda(apt)
                             (if (list? apt)
                                 (string? (last apt))
                                 #f)))


#|
Functions for add and deleting appointments and calendars from a calendar.
|#

;;add appointment to the end of a calendar
(define addAppointmentToCalendar( lambda(cal apt)
                                   (append cal (list apt))))

;;add calendar to the end of a calendar
(define addCalendarToCalendar( lambda(cal addCal)
                                (addAppointmentToCalendar cal addCal)))

;;delete appointment, returns the resulting flattened calendar. Update to support nested calendars!!!
(define deleteAppointmentFromCalendar( lambda(cal apt)
                                        (reverse (deleteAppointmentHelper cal apt '()))
                                        ))

(define deleteAppointmentHelper( lambda(cal apt res)
                                  (if (empty? cal)
                                      res
                                      (cond ((checkIfCalendar? cal) (deleteAppointmentHelper (cdr cal) apt (cons (car cal) res))) ;;too lazy to check inside nested calendar
                                            ((checkIfCalendar? (car cal)) (deleteAppointmentHelper (cdr cal) apt (cons (car cal) res)))
                                            ((checkIfAppointment? (car cal))
                                             (if (and
                                                  (eqv? (calcTimeSeconds (getStartTime (car cal))) (calcTimeSeconds (getStartTime apt)))
                                                  (eqv? (calcTimeSeconds (getStartTime (cdr (car cal)))) (calcTimeSeconds (getEndTime apt)))
                                                  (equal? (car (cdr (cdr (car cal)))) (getContent apt))
                                                  )
                                                 (deleteAppointmentHelper (cdr cal) apt res) ;;delete the appointment by not adding it again
                                                 (deleteAppointmentHelper (cdr cal) apt (cons (car cal) res)) ;;add it
                                              ))))))

(define deleteCalendarFromCalendar( lambda(cal delCal)
                         (reverse (deleteCalendarHelper cal delCal '()))
                         ))

(define deleteCalendarHelper( lambda(cal delCal res)
                                  (if (empty? cal)
                                      res
                                      (cond ((checkIfCalendar? cal) (deleteCalendarHelper (cdr cal) delCal (cons (car cal) res))) ;;too lazy to check inside nested calendar
                                            ((checkIfCalendar? (car cal)) (if (equal? (car cal) delCal)
                                                                              (deleteCalendarHelper (cdr cal) delCal res)
                                                                              (deleteCalendarHelper (cdr cal) delCal (cons (car cal) res))))                                                                              
                                            ((checkIfAppointment? (car cal)) (deleteCalendarHelper (cdr cal) delCal (cons (car cal) res))) 
                                             ))))




#|
Functions for converting the provided calendar within a time-interval to html
|#
(define present-calendar-html( lambda(cal from-time to-time)
                                (create-calendar-html (get-appointments-in-interval (parseCalendar cal1 '()) from-time to-time '()) from-time to-time "") ;;start with sorting the from-time to-time, then create html from the returned list
                                )) 

(define get-appointments-in-interval( lambda(cal from-time to-time res)
                                 (if (empty? cal)
                                     res
                                     (if (appointmentsInInterval? (car cal) from-time to-time)
                                         (get-appointments-in-interval (cdr cal) from-time to-time (cons (car cal) res))
                                         (get-appointments-in-interval (cdr cal) from-time to-time res)))))

;; checks if the provided appointment apt takes place in the provided time-interval
;; from-time < aptStartTime < aptEndTime < to-time
(define appointmentsInInterval?( lambda(apt from-time to-time)
                                  (let* ([aptStartTime (getStartTime apt)]
                                         [aptEndTime (getEndTime apt)])
                                    (if (and
                                         (< (calcTimeSeconds from-time) (calcTimeSeconds aptStartTime)) ;; from-time < aptStartTime
                                         (< (calcTimeSeconds aptStartTime) (calcTimeSeconds aptEndTime)) ;; aptStartTime < aptEndTime
                                         (< (calcTimeSeconds aptEndTime) (calcTimeSeconds to-time))) ;; aptEndTime < to-time
                                        #t
                                        #f
                                        ))))

;;first constructs the first row of the html table and then adds the rest as it's constructed
(define create-calendar-html( lambda(cal from-time to-time res)
                               (if (empty? cal)
                                   (html ""
                                         (head "" "" "")
                                         (body ""
                                               (table "border='1' style='width:100%'"
                                                      (tr ""
                                                          (td "" "Appointment content" "")
                                                          (td "" "from-time: " (convert-time-toString from-time))
                                                          (td "" "to-time: " (convert-time-toString to-time)))
                                                      res)))
                                   (create-calendar-html (cdr cal) from-time to-time (string-append (appointment-to-html (car cal)) res))
                               )))

(define appointment-to-html( lambda(apt)                                                  
                              (tr ""
                                  (td "" (getContent apt) "")
                                  (td "" (convert-time-toString (getStartTime apt)) "")
                                  (td "" (convert-time-toString (getEndTime apt)) "")) ;;this is not getting catched                                             
                          ))

(define convert-appointment-toString( lambda(apt)
                                       (string-append (convert-time-toString (getStartTime apt)) " to " (convert-time-toString (getEndTime apt)) " " (getContent apt))
                                       ))

(define convert-time-toString( lambda(time)
                                (string-append (number->string (getDay time)) "/" (number->string (getMonth time)) "/" (number->string (getYear time)) " " (number->string (getHour time)) ":" (number->string (getMinute time)))
                                ))


#|
Functions for finding appointments based on difference predicates
|#
(define find-appointments (lambda(cal pred)
                            (reverse (filter pred (parseCalendar cal '())))
                            ))

;;find the earliest appointment based on predicate pred
(define find-first-appointment (lambda(cal pred)
                                 (find-filtered-appointment (filter pred (parseCalendar cal '())) "first" '())
                                 ))

;;find the latest appointment based on predicate pred
(define find-last-appointment (lambda(cal pred)
                                 (find-filtered-appointment (filter pred (parseCalendar cal '())) "last" '())
                                 ))


(define find-filtered-appointment (lambda(cal marker res)
                                    (if (empty? cal)
                                        res
                                        (if (empty? res)
                                             (find-filtered-appointment (cdr cal) marker (car cal))
                                             (cond ((equal? marker "first") (if (is-first-appointment? (car cal) res)
                                                                                (find-filtered-appointment (cdr cal) marker (car cal))
                                                                                (find-filtered-appointment (cdr cal) marker res)))
                                                   ((equal? marker "last") (if (is-latest-appointment? (car cal) res)
                                                                               (find-filtered-appointment (cdr cal) marker (car cal))
                                                                               (find-filtered-appointment (cdr cal) marker res)
                                                                               )))))))

;;returns true if the startTime of the provided appointment is smaller than the startTime of the currently stored appointment
;;else it returns false
(define is-first-appointment? (lambda(apt res)
                             (cond ((< (calcTimeSeconds (getStartTime apt)) (calcTimeSeconds (getStartTime res))) #t)
                                   ((> (calcTimeSeconds (getStartTime apt)) (calcTimeSeconds (getStartTime res))) #f)
                                   )))

;;returns the opposite of is-first-appointment?.
;;Notice that in case the appointment startTimes are equal this will result in overriden res for is-latest-appointment? but not for is-first-appointment?.
(define is-latest-appointment? (lambda(apt res)
                                 (not (is-first-appointment? apt res))
                                 ))

#|
Function for flattening a calendar
|#
;;returns a single flattened calendar storing all appointments
(define flatten-calendar (lambda(cal)
                           (cons "calendar" (parseCalendar cal '())))) 


#|
Functions for checking whether two appointsment or two calendars overlap
|#
(define appointments-overlap? (lambda(ap1 ap2)
                                (if (and (checkIfAppointment? ap1) (checkIfAppointment? ap2))
                                    (cond ((< (calcTimeSeconds (getEndTime ap1)) (calcTimeSeconds (getStartTime ap2))) #f) ;;if ap1 ends before ap2 starts there can't be overlap
                                          ((< (calcTimeSeconds (getEndTime ap2)) (calcTimeSeconds (getStartTime ap1))) #f) ;;if ap2 ends before ap1 starts there can't be overlap
                                          ((and (= (calcTimeSeconds (getStartTime ap1)) (calcTimeSeconds (getStartTime ap2))) (= (calcTimeSeconds (getEndTime ap1)) (calcTimeSeconds (getEndTime ap2)))) #t)
                                          (else #t)
                                          )
                                    #f)))

(define calendars-overlap? (lambda(cal1 cal2)
                             (calendars-overlapH (parseCalendar cal1 '()) (parseCalendar cal2 '()) (parseCalendar cal2 '()))
                             ))

(define calendars-overlapH (lambda(cal1 cal2 cal2Copy)
                             (if (empty? cal1)
                                 #f
                                 (calendars-overlap-iter cal1 cal2 cal2Copy)
                                 )))

;;iterates through the calendars and returns true if two appointments within the two calendars overlap
(define calendars-overlap-iter (lambda(cal1 cal2 cal2Copy)
                                 (if (equal? (appointments-overlap? (car cal1) (car cal2)) #t)
                                     #t
                                     (if (empty? cal2)
                                         (calendars-overlapH (cdr cal1) cal2Copy cal2Copy) ;;go to next appointment in cal1 and start over on cal2
                                         (calendars-overlap-iter cal1 (cdr cal2) cal2Copy) ;;loop cal2
                                         ))))


#|
-----------------------------------
***Testing functionality section***
-----------------------------------
|#

#|
Creating test calendars
|#
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
              (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content3") 
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass6")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass7")
               (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content4") 
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass8")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass9")))
              (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "pass10")))


(define cal2 (createCalender
              (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content1")
               (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content2") 
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass1")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass2"))
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass3")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass4"))
              (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 56) "pass5")   
              (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "pass10")))

(define cal3 (createCalender
              (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content1")
               (createCalender
                (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass2"))
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass4"))
              (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 56) "pass5")))


(define cal4 (createCalender              
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content1")               
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content2") 
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "my content3")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "my content4")
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "my content5")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "my content6")
              (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 56) "my content7")   
              ))



#|
Testing flatten calendar
|#
;cal1
;cal2
;(parseCalendar cal1 '())
;(flatten-calendar cal1)
"-----------------------------------------"


;(appointmentsInInterval? (car (cdr (parseCalendar cal1 '()))) (createTime 2005 11 24 10 55) (createTime 2005 11 24 20 54))
;(present-appointments-in-interval cal1 (createTime 2005 11 24 10 55) (createTime 2005 11 24 20 54) '())


 
#|
Test add and delete appointments and calendars
|#
;(addAppointmentToCalendar cal1 (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content22"))
#|
(addCalendarToCalendar cal1 (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content") 
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass2")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass1")))
|#
;cal2
"------------------------------------------------------------------------------------------------------------"
;(deleteAppointmentFromCalendar cal2 (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 56) "pass5") )
#|
(deleteCalendarFromCalendar cal1 (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content3") 
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass6")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass7")
               (createCalender
               (createAppointment (createTime 2005 11 24 23 55) (createTime 2005 11 24 23 56) "my content4") 
               (createAppointment (createTime 2005 11 24 15 55) (createTime 2005 11 24 16 54) "pass8")
               (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass9"))))
              |#

#|
Test different find-appointment functions
|#
;(find-appointments (parseCalendar cal1 '()) list?)
;(find-first-appointment (parseCalendar cal1 '()) list?)
;(find-last-appointment (parseCalendar cal1 '()) list?)


#|
Test appointment and calendar overlap function
|#
;(appointments-overlap? (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 56) "pass3") (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 56) "pass3"))
;(calendars-overlap? cal1 cal2)

#|
Test present-calendar-html and other html function 
|#
;(present-calendar-html cal1 (createTime 2005 11 24 11 58) (createTime 2005 11 24 23 59))
;(appointment-to-html (createAppointment (createTime 2005 11 24 11 55) (createTime 2005 11 24 13 54) "pass9") (createTime 2005 11 24 11 58) (createTime 2005 11 24 23 59))

#|
(define present-weekly-calendar( lambda()

                                  ))
|#

#|
"------------------------------------------------------------------------------"
(define create-weekly-calendar-html( lambda(cal from-time to-time counter res)
                                      (if (empty? cal)                                         
                                          (html ""
                                                (head "" "" "")
                                                (body ""
                                                      (table "border='1' style='width:100%'"
                                                             (tr ""
                                                                 (td "" "Appointment content" "")
                                                                 (td "" "from-time: " (convert-time-toString from-time))
                                                                 (td "" "to-time: " (convert-time-toString to-time)))                                                            
                                                             res
                                                             )))
                                          ;
                                              (create-weekly-calendar-html (cdr cal) from-time to-time counter ;;start with tr
                                                                           (string-append (create-week-table-html (car cal) (constructDateList 1 '()) #f counter res) res))
                                               ;;just do single tds
                                                                           
                                              
                                          
                               )))

(define constructDateList( lambda(currDate res)
                   (if (< currDate 32)
                       (constructDateList (+ currDate 1) (cons (list currDate 11) res))
                       (reverse res)
                       )))

(define create-week-table-html( lambda(cal dates tdBool counter res)
                                 (cond ((empty? dates) res)
                                       ((equal? tdBool #t) 
                                        (create-week-table-html cal (cdr dates) #t (+ counter 1) (construct-td  (car (car dates))) (car (cdr (car dates)))))) ;;add td
                                       ((or (equal? counter 1) (equal? counter 8) (equal? counter 15) (equal? counter 22) (equal? counter 29))
                                        (create-week-table-html cal dates tdBool (+ counter 7) (tr "" (construct-td (car (car dates)) (car (cdr (car dates))) (+ counter 7))))                             
                                        ) ;;construct tds for the 7 next days

                                       ;(else (create-week-table-html cal (cdr dates) #t (+ counter 1) (construct-td (car (car dates)) (car (cdr (car dates))))))
                                 ))


(define construct-td( lambda(day month counter)
                       
                       (td "" (string-append (number->string day) "/" (number->string month)) "")
                       ))

"---------------------------------"
|#
;cal4
;(create-weekly-calendar-html cal1 (createTime 2005 11 24 11 58) (createTime 2005 11 24 23 59) 1 "")