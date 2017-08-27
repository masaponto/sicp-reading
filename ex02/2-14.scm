(use gauche.test)


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

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percent)
  (make-interval (- center (* center (/ percent 100.0)))
                 (+ center (* center (/ percent 100.0)))))

(test* "make-center-percent: 1"
       (make-interval 99.0 101.0)
       (make-center-percent 100 1))

(test* "make-center-percent: 2"
       (make-interval 95.0 105.0)
       (make-center-percent 100 5))

(display (make-center-percent 100 5))

;;(define (percent i)
;;  (let ((t (/ (upper-bound i) (center i))))
;;    (* (- t 1) 100)))

(define (percent i)
  (* (/ (width i) (center i)) 100))

(test* "percent: 1"
       5.0
       (percent (make-center-percent 100 5)))

(test* "percent: 2"
       1.0
       (percent (make-center-percent 100 1)))

(test* "percent: 3"
       100.0
       (percent (make-center-percent 100 100)))

(display (percent (make-center-percent 100 5)))
(display (percent (make-interval 95 105)))

(mul-interval (make-center-percent 100 5) (make-center-percent 100 10))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(let ((a (make-center-percent 100 1.2))
      (b (make-center-percent 100 1.5)))
  (display (par1 a b))
  (display (par2 a b))

(let ((a (make-interval 10 15))
      (b (make-interval 10 20)))
  (display (par1 a b))
  (display (par2 a b)))

(mul-interval
 (div-interval (make-interval 1 1)
               (make-interval 1 2))
 (make-interval 1 2))


;;(par1 (make-interval 5 10) (make-interval 5 10))
;;(par2 (make-interval 5 10) (make-interval 5 10))
