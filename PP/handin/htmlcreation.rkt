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
                        "the format of year is unacceptable")))

(define checkMonth (lambda (month)
                     (if (and (> month 0) (< month 13))
                         month
                         "the format of month is unacceptable")))

(define checkDay (lambda (day)
                   (if (and (> day 0) (< day 32))
                       day
                       "the format of day is unacceptable")))

(define checkHour (lambda (hour)
                    (if (and (> hour -1) (< hour 24))
                        hour
                        "the format of hour is unacceptable")))

(define checkMinute (lambda (minute)
                      (if (and (> minute -1) (< minute 61))
                          minute
                          "the format of minute is unacceptable")))

#|
Functions for getting different elements from the appointment time list
|#
(define get-year first)
(define get-month second)
(define get-day third)
(define get-hour fourth)
(define get-minute fifth)

#|
Internal calender representation: The root is a calender which is a list of appointments
|#

(define createAppointment( lambda(time content)
                            (list time content)))

(createAppointment (createTime 2005 11 24 23 55) "my content")                              





;; do something to time to calc time since date
(define calcTimeSinceChineseNY( lambda(time) 's))


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

(findAttributeInLst (list (list 's 'b 'd) 'd 3 'g) '())
;(findContentInLst (list 's "asf" 3 'g) '())