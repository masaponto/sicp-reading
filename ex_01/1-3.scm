#lang racket

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (hoge a b c)
  (cond ((or (> a b c) (> b a c)) (sum-of-squares a b))
        ((or (> a c b) (> c a b)) (sum-of-squares a c))
        ((or (> b c a) (> c b a)) (sum-of-squares b c))))

(hoge 1 2 3)
(hoge 2 3 1)
(hoge 3 2 1)
(hoge 1 3 2)
