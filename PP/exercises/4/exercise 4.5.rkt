#lang racket
(define(cmp lt gt)
  (lambda (x y)
  (cond
    ((lt x y) -1)   
    ((gt x y) 1)
    (else 0)
    )
  ))
;;((cmp < >) 1 2)

;; example
;;(not (> 4 4))

(define(shortcmp lt)
  (lambda (x y)
  (cond
    ((eq? x y) 0)
    ((lt x y) -1)   
    (else 1)
    )
  ))

;;((shortcmp <) 5 4)

;; string compare
(define strcmp
  (cmp string<? string>?)
 )
;;(strcmp "6" "5")


(define shortstrcmp
  (shortcmp string<?)
 )
;;(shortstrcmp "5" "4")


(define (mkList l1 l2 l3)
  ;;(lambda (x y z)
    (list l1 l2 l3)
    
    )

(mkList < = >)

