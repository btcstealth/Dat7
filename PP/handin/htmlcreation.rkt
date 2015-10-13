#lang racket
(define tagCreator(lambda(tag)
                    (lambda(attributes . contents)
                      (helpFunc contents tag))))

(define helpFunc(lambda(cont tag)
                  (cond ((null? cont) "")
                            ((string? cont) (string-append "<"tag">" cont "</"tag">"))
                            ;((list? cont) (string-append "<"tag">" (car cont) (list-ref cont 1) "</"tag">"))
                            ((procedure? cont) (string-append cont))
                            ((string? (car cont)) (string-append (car cont)) (helpFunc (cdr cont) tag))
                  )))

(define html(tagCreator "html"))
(define head(tagCreator "head"))
(define body(tagCreator "body"))
(define table(tagCreator "table"))
(define span(tagCreator "span"))
(define h1(tagCreator "h1"))



(html "attributes"
      (head "attributes" "test" "")
      (body "attributes" "" ""))


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

