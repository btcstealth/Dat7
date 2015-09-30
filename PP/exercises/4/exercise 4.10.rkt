#lang racket
(define (for-all lst p)
  (if (null? lst) #t
      (if (p (car lst)) (for-all (cdr lst) p) #f)
  ))

;;(for-all (list 1 2 3) number?)

(define (there-exists lst p)
  (if (null? lst) #f
      (if (p (car lst)) #t (there-exists (cdr lst) p))
   ))

;;(there-exists (list 'a 'b 3 4) number?)


(define (there-exists-1 lst p counter)
  (if (null? lst)
      (counterFunc counter)
      (if (p (car lst)) (there-exists-1 lst p (+ counter 1)) (there-exists-1 (cdr lst) p counter))
   )
  )

(define (counterFunc counter)
  (if (= counter 1) #t #f) 
  )

(there-exists-1 (list 1 3 5 6) number? 0)