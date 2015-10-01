#lang racket


(define (p-cps a b k0)
  (plus a b (lambda(v1)
              ; given the sum of and b in v1, now compute a-b and
              ; the product. 
              (sub a b (lambda(v2)
                         ; given the sum in v1 and the difference in v2, now 
                         ; carry out the rest of the computation (the multiplication)
                         ; and pass the result to k0.
                         (mult v1 v2 k0))))))



(define (discriminant a b c k0)
  (square b (lambda(v1) v1)
          (mult )
          ))

  ;(sub (square b) (mult (mult 4 a) c)))


; AUX functions:
(define (square a k)
  (k (mult a a (lambda(x) x))))

(define (mult a b k)
  (k (* a b)))

(define (sub a b k)
  (k (- a b)))