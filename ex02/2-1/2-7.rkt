#lang racket
(require rackunit)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;;(make-interval 100 6))

;;(check-equal?
;; (new-mul-interval (make-interval -3 10) (make-interval 2 10))
;; (make-interval - 6))



(define (div-interval x y)
  (mul-interval
   x
   (if (>= (* (upper-bound y) (lower-bound y)) 0)
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y)))
       (error "ゼロを跨ぐ区間です"))))

;; 2.7
(define (make-interval a b) (cons a b))

(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))

;; 2.8
(define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
                 (- (lower-bound x) (upper-bound y))))

(check-equal?
 (sub-interval (make-interval 10 3) (make-interval 5 2))
 (cons 8 -2))


;; 2.9
;;(define (get-width x) (/ (- (upperbound x) (lowerbound x)) 2))

;;(define (add-interval w z)
;;  )

;;```
;;a < b, c < d
;;(a, b) -> (b-a)/2 = t
;;(c, d) -> (d-c)/2 = s
;;(a+c, b+d) -> {b+d-(a+c)}/2 ={(b-a)/2} + {(d-c)/2} = t + s
;;```
;;
;;```
;;a < b, c < d
;;(a, b) -> (b-a)/2 = t
;;(c, d) -> (d-c)/2 = s
;;(a-d, b-c) -> {b-c-(a-d)}/2 = {(b-a)/2} + {(d-c)/2} = t + s
;;```
;;
;;```
;;a < b, c < d
;;(a, b) -> (b-a)/2 = t
;;(c, d) -> (d-c)/2 = s
;;(a-d, b-c) -> {b-c-(a-d)}/2 = {(b-a)/2} + {(d-c)/2} = t + s
;;```
;;
;;```
;;a < b, c < d
;;(a, b) -> (b-a)/2 = t
;;(c, d) -> (d-c)/2 = s
;;
;;a < b < c < d
;;
;;p1 = a*c
;;p2 = a*d
;;p3 = b*c
;;p4 = b*d
;;
;;(a*c, b*d) -> {b*d-(a*c)}/2 =
;;```


;;
(define (check-range x)
  (cond ((and (< 0 (lower-bound x)) (< 0 (upper-bound x))) 0)
        ((and (< (lower-bound x) 0) (< 0 (upper-bound x))) 1)
        ((and (< (lower-bound x) 0) (< (upper-bound x) 0)) 2)))

(define (new-mul-interval x y)
  (cond
   ((and (= (check-range x) 0) (= (check-range y) 0))
    (make-interval (* (lower-bound x) (lower-bound y))
                   (* (upper-bound x) (upper-bound y))))

   ((and (= (check-range x) 1) (= (check-range y) 0))
    (make-interval (* (lower-bound x) (upper-bound y))
                   (* (upper-bound x) (upper-bound y))))

   ((and (= (check-range x) 2) (= (check-range y) 0))
    (make-interval (* (lower-bound x) (upper-bound y))
                   (* (upper-bound x) (lower-bound y))))

   ((and (= (check-range x) 0) (= (check-range y) 1))
    (make-interval (* (upper-bound x) (lower-bound y))
                   (* (upper-bound x) (upper-bound y))))

   ((and (= (check-range x) 1) (= (check-range y) 1))
    (let ((a (* (lower-bound x) (lower-bound y)))
          (b (* (lower-bound x) (upper-bound y)))
          (c (* (upper-bound x) (lower-bound y)))
          (d (* (upper-bound x) (upper-bound y))))
      (make-interval (min b c)
                     (max a d))))

   ((and (= (check-range x) 2) (= (check-range y) 1))
    (make-interval (* (lower-bound x) (upper-bound y))
                   (* (lower-bound x) (lower-bound y))))

   ((and (= (check-range x) 0) (= (check-range y) 2))
    (make-interval (* (upper-bound x) (lower-bound y))
                   (* (lower-bound x) (upper-bound y))))

   ((and (= (check-range x) 1) (= (check-range y) 2))
    (make-interval (* (upper-bound x) (lower-bound y))
                   (* (lower-bound x) (lower-bound y))))

   ((and (= (check-range x) 2) (= (check-range y) 2))
    (make-interval (* (upper-bound x) (upper-bound y))
                   (* (lower-bound x) (lower-bound y))))))


(define (test-new-mulinterval)
  (let ((x1 (make-interval 2 3))
        (x2 (make-interval -2 3))
        (x3 (make-interval -2 -3)))

    (check-equal? (check-range x1) 0)

    (check-equal? (check-range x2) 1)

    (check-equal? (check-range x3) 2)

    (check-equal?
     (upper-bound (new-mul-interval x1 x1)) 9)

    (check-equal?
     (lower-bound (new-mul-interval x1 x1)) 4)

    (check-equal?
     (upper-bound (new-mul-interval x1 x2)) 9)

    (check-equal?
     (lower-bound (new-mul-interval x1 x2)) -6)

    (check-equal?
     (upper-bound (new-mul-interval x2 x1)) 9)

    (check-equal?
     (lower-bound (new-mul-interval x2 x1)) -6)

    (check-equal?
     (upper-bound (new-mul-interval x1 x3)) -4)

    (check-equal?
     (lower-bound (new-mul-interval x1 x3)) -9)

    (check-equal?
     (upper-bound (new-mul-interval x3 x1)) -4)

    (check-equal?
     (lower-bound (new-mul-interval x3 x1)) -9)
    ))

(test-new-mulinterval)
