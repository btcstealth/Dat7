#lang racket



(define (discriminant a b c)
  (sub (square b) (mult (mult 4 a) c)))


; AUX functions:
(define (square a k)
  (k (mult a a (lambda(x) x))))

(define (mult a b k)
  (k (* a b)))

(define (sub a b k)
  (k (- a b)))