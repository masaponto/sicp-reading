#lang racket

(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

;;(lambda (f)
;;  (lambda (x)
;;    (f (((lambda (f) (lambda (x) x)) f) x))))
;;
;;(lambda (f)
;;  (lambda (x)
;;    (f (((lambda (x) x)) x))))
;;
;; this is one
;;(lambda (f)
;;  (lambda (x)
;;    (f x)))

(define one
  (lambda (f)
    (lambda (x) (f x))))
;; ((one (lambda(x) (+ 1 x))) 0) => 1


;; (lambda (f)
;;   (lambda (x)
;;     (f (((lambda (f) (lambda (x) f x)) f) x))))
;;
;;(lambda (f)
;;   (lambda (x)
;;     (f ((lambda (x) f x) x))))
;;
;; this is two
;;(lambda (f)
;;   (lambda (x)
;;     (f (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

;; ((two (lambda(x) (+ 1 x))) 0) => 2

(define (plus p q)
  (lambda (f)
    (lambda (x)
      ((p f) ((q f) x)))))

;; this is miss
;;(define (plus- p q)
;;  (lambda (f)
;;    (lambda (x)
;;      ((p (q f) x)))))

(define (inc x)
  (+ 1 x))
