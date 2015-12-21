#lang racket

;; 1-2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))


;; 1-3
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


;; 1-4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 3 -2)
(a-plus-abs-b 3 2)

;; 1-5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))
