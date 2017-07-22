#!/usr/bin/racket
#lang racket

(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define tolerance 0.00001)



(define (fixed-point f first-guess)
  (define count 0)

  (define (close-enough? v1 v2)
    (< (abs (- v1 v2 ))
       tolerance ))

  (define (try guess)
    (let ((next (f guess)))
      (set! count (+ count 1))
      (writeln next)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess)
  (writeln count))


;;(fixed-point cos 1.0)

;; ex 1.35
;;(fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1)

;; ex 1.36
(fixed-point (lambda (x) (/ (log 1000) (log x))) 0.01)
