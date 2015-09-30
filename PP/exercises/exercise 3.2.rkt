#lang racket

(define (replicate lst1 n)
  (replicate-to-length lst1 n 1 '())
  )

(define (replicate-to-length lst1 n counter res)
  (cond ((< n counter) (reverse res))
        ((null? lst1) (replicate-to-length (reverse res) n counter res))
  (else (replicate-to-length (cdr lst1) n (+ counter 1) (cons (car lst1) res))))
  )

(replicate '(a b c) 7)