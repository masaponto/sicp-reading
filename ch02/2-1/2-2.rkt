#lang racket

(define (make-segment p q)
  (cons p q))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (mid-point-segment s)
  (let* ((p (start-segment s))
         (q (end-segment s)))
    (make-point (/ (+ (x-point p) (x-point q)) 2)
                (/ (+ (y-point p) (y-point q)) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))


(define s (make-segment (make-point 4.0 4.0) (make-point -4.0 -4.0)))
(print-point (start-segment s))
(print-point (end-segment s))
(print-point (mid-point-segment s))
(print-point (mid-point-segment (make-segment (make-point 4 4) (make-point 0 0))))

;; 2-3

;;(define (left-top r)
;;  (car r))

;;(define (right-bottom r)
;;  (cdr r))

(define (make-rectangle p q) ;; point, point
  (make-segment p q))

(define (rectangle-perimeter r)
  (let* ((p (start-segment r))
         (q (end-segment r))
         (x (x-point p))
         (y (y-point p))
         (z (x-point q))
         (w (y-point q)))
    (* 2 (+ (abs (- z x)) (abs (- w y))))))

(define (rectangle-area r)
  (let* ((p (start-segment r))
         (q (end-segment r))
         (x (x-point p))
         (y (y-point p))
         (z (x-point q))
         (w (y-point q)))
    (* (abs (- z x)) (abs (- w y)))))

;;(define s (make-segment (make-point 4.0 4.0) (make-point -4.0 -4.0)))

(define r (make-rectangle (make-point 4.0 4.0) (make-point -4.0 -4.0)))

(display (rectangle-perimeter r))
(newline)
(display (rectangle-area r))

(newline)

(define r2 (make-rectangle (make-point 4.0 0.0) (make-point -4.0 0)))

(display (rectangle-perimeter r2))
(newline)
(display (rectangle-area r2))
(newline)
