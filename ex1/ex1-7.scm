#lang racket

(define (sqrt-iter old-guess new-guess x)
  (if (good-enough? old-guess new-guess x)
      new-guess
      (sqrt-iter new-guess (improve new-guess x)
                 x)))

(define (improve x y)
  (average x (/ y x)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? old-guess new-guess x)
  (< (abs (- 1.0 (/ old-guess new-guess))) 0.001))

;;(define (good-enough? guess x)
;; (< (abs (- (square guess) x)) 0.001))

;; (define (square x)
;;         (x * x) )

(define (sqrt- x)
  (sqrt-iter 1.0 x x))
