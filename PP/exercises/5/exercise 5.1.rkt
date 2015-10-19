#lang racket
(define (point x y)
  (letrec ((getx    (lambda () x))
           (gety    (lambda () y))
           (add     (lambda (p) 
                      (point 
                       (+ x (send 'getx p))
                       (+ y (send 'gety p)))))
           (type-of (lambda () 'point))
           ;;move function
           ;(move (lambda () dx dy) (getx ))
          )
    (lambda (message)
      (cond ((eq? message 'getx) getx)
            ((eq? message 'gety) gety)
            ((eq? message 'add)  add)
            ((eq? message 'type-of) type-of)
            ;((eq? message 'move) move)
            (else (error "Message not understood"))))))


(define (send message obj . par)
  (let ((method (obj message)))
    (apply method par)))


;(send 'getx (point 5 7))

;x + x
;(send 'getx (send 'add (point 5 7) (point 5 3)))

;; create two lists with four points each
(define (append-lists lst1 lst2)
    (append-lists-helper lst1 lst2 '()))

(define append-lists-helper(lambda(lst1 lst2 res)
    (if (or (null? lst1) (null? lst2)) res
        (append-lists-helper (cdr lst1) (cdr lst2) (cons (send 'add (car lst1) (point 5 3)) res ))
    )
  ))


((lst1)(point 2 5) (point 10 5) (point 3 5) (point 5 5))
