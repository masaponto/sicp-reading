#lang racket

(define (cons- x y)
  (* (expt 2 x) (expt 3 y)))

(define (car p)
  (diver p 2 0))

(define (cdr p)
  (diver p 3 0))

(define (diver p x c)
  (cond ((= (modulo p x) 0) (diver (/ p x) x (+ c 1)))
        (else c)))


(display (car (cons- 2 3)))
(newline)
(display (cdr (cons- 3 4)))
(newline)

(display (cdr (cons- 5 4)))
(newline)

(display (car (cons- 5 10)))
(newline)
