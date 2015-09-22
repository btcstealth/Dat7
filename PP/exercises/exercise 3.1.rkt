#lang racket
(define(test arg1 arg2)
  (+ arg1 arg2)
  )

(test 2 4)



(define (my-next-append lst1 lst2)
    (my-next-append-1 lst1 lst2 '()))


(define (my-next-append-1 lst1 lst2 res)
    (cond ((null? lst2) (reverse res))
           ((null? lst1) (my-next-append-1 lst1 (cdr lst2) (cons (car lst2) res))) ;; end condition
          (else (my-next-append-1 (cdr lst1) lst2 (cons (car lst1) res)))))



(my-next-append (list 1 3 5) (list 6 7 9))