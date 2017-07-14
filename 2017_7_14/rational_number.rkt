#lang racket

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let* ((g (gcd n d))
	 (a (/ n g))
	 (b (/ d g)))
    (cond ((and (< a 0) (< b 0)) (cons (abs a) (abs b)))
	  ((and (< a 0) (> b 0)) (cons (abs a) (* -1 b)))
	  (else (cons a b)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)
;;
(print-rat (make-rat -2 -4))
(print-rat (make-rat -2 -4))
(print-rat (make-rat -2 4))
(newline)
