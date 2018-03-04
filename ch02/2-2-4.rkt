#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; How to evaluate
;; C-c C-k

;; Reference: http://uents.hatenablog.com/entry/sicp/011-ch2.2.4.1.md
(define wave
  (segments->painter
   (list (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
         (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
         (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
         (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
         (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
         (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
         (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
         (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
         (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
         (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
         (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
         (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
         (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
         (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0)))))

(define wave2 (beside wave (flip-vert wave)))

(define (flipped-pairs painter)
  (let ((painter2 ( beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

;; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


;; 2.45
(define (split f g)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split f g) painter (- n 1))))
          (f painter (g smaller smaller))))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside ( flip-horiz quarter ) quarter)))
      (below (flip-vert half) half))))


(define (square-of-four tl tr bl br)
(lambda (painter)
  (let ((top (beside (tl painter) (tr painter)))
        (bottom (beside (bl painter) (br painter))))
    (below bottom top))))

(define (flipped-pairs2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;; 2.46

(define (make-vect2 x y)
  (cons x y))

(define (xcor-vect2 vect)
  (car vect))

(define (ycor-vect2 vect)
  (cdr vect))

(define (add-vect2 a b)
  (make-vect2 (+ (xcor-vect2 a) (xcor-vect2 b))
              (+ (ycor-vect2 a) (ycor-vect2 b))))

(define (sub-vect2 a b)
  (make-vect2 (- (xcor-vect2 a) (xcor-vect2 b))
              (- (ycor-vect2 a) (ycor-vect2 b))))

(define (scale-vect2 s vect)
  (make-vect2 (* s (xcor-vect2 vect))
              (* s (ycor-vect2 vect))))


(require rackunit)

(check-equal?
 (make-vect2 2 3)
 '(2 . 3))

(check-equal?
 (xcor-vect2 (make-vect2 2 3))
 2)

(check-equal?
 (ycor-vect2 (make-vect2 2 3))
 3)

(check-equal?
 (add-vect2 (make-vect2 2 3) (make-vect2 3 4))
 (make-vect2 5 7))

(check-equal?
 (sub-vect2 (make-vect2 2 3) (make-vect2 3 4))
 (make-vect2 -1 -1))

(check-equal?
 (scale-vect2 2 (make-vect2 3 4))
 (make-vect2 6 8))


;; 2.47
(define (make-frame2 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame2 frame)
  (first frame))

(define (edge1-frame2 frame)
  (second frame))

(define (edge2-frame2 frame)
  (third frame))

(define (make-frame3 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame3 frame)
  (car frame))

(define (edge1-frame3 frame)
  (cadr frame))

(define (edge2-frame3 frame)
  (cddr frame))

(let ((frame (make-frame2 (make-vect2 0.0 0.0)
                          (make-vect2 2.0 0.0)
                          (make-vect2 0.0 2.0))))
  (check-equal?
   (origin-frame2 frame)
   (make-vect2 0.0 0.0))

  (check-equal?
   (edge1-frame2 frame)
   (make-vect2 2.0 0.0))

  (check-equal?
   (edge2-frame2 frame)
   (make-vect2 0.0 2.0)))

(let ((frame (make-frame3 (make-vect2 0.0 0.0)
                          (make-vect2 2.0 0.0)
                          (make-vect2 0.0 2.0))))
  (check-equal?
   (origin-frame3 frame)
   (make-vect2 0.0 0.0))

  (check-equal?
   (edge1-frame3 frame)
   (make-vect2 2.0 0.0))

  (check-equal?
   (edge2-frame3 frame)
   (make-vect2 0.0 2.0)))


;; 2.48
(define (make-segment2 start end)
  (cons start end))

(define (start-segment2 segment)
  (car segment))

(define (end-segment2 segment)
  (cdr segment))

;; 2.49
;; a
(define (frame-painter tr br tl bl)
  (segments->painter
   (list (make-segment2 tr br)
         (make-segment2 br bl)
         (make-segment2 bl tl)
         (make-segment2 tl tr))))

;; b
(define (x-painter tr br tl bl)
  (segments->painter
   (list (make-segment2 tr bl)
         (make-segment2 tl br))))
#|
(let ((tr (make-vect 0.5 0.5))
      (br (make-vect 0.5 0.1))
      (tl (make-vect 0.1 0.5))
      (bl (make-vect 0.1 0.1)))
  (paint-hires (frame-painter tr br tl bl)))

(let ((tr (make-vect 0.5 0.5))
      (br (make-vect 0.5 0.1))
      (tl (make-vect 0.1 0.5))
      (bl (make-vect 0.1 0.1)))
  (paint-hires (x-painter tr br tl bl)))
|#

;; c
(define (mid-point a b)
  (make-vect2 (/ (+ (xcor-vect2 a) (xcor-vect2 b)) 2)
              (/ (+ (ycor-vect2 a) (ycor-vect2 b)) 2)))

(define (diamond-painter tr br tl bl)
  (let ((a (mid-point tr br))
        (b (mid-point br bl))
        (c (mid-point bl tl))
        (d (mid-point tl tr)))
    (segments->painter
     (list (make-segment2 a b)
           (make-segment2 b c)
           (make-segment2 c d)
           (make-segment2 d a)))))

#|
(let ((tr (make-vect 0.5 0.5))
      (br (make-vect 0.5 0.1))
      (tl (make-vect 0.1 0.5))
      (bl (make-vect 0.1 0.1)))
  (paint-hires (diamond-painter tr br tl bl)))
|#

(check-equal?
 (mid-point (make-vect2 0.0 0.0) (make-vect2 1.0 1.0))
 (make-vect2 0.5 0.5))

(check-equal?
 (mid-point (make-vect2 0.5 0.5) (make-vect2 0.5 0.1))
 (make-vect2 0.5 0.3))


;; 2.50
(define (transform-painter2 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect2 (m corner1) new-origin)
                  (sub-vect2 (m corner2) new-origin)))))))

(define (flip-horiz2 painter)
  (transform-painter2 painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate-180 painter)
  (transform-painter2 painter
                      (make-vect 1.0 1.0)
                      (make-vect 0.0 1.0)
                      (make-vect 1.0 0.0)))

(define (rotate-270 painter)
  (transform-painter2 painter
                      (make-vect 0.0 1.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0)))

#|
(paint-hires wave)
(paint-hires (flip-horiz2 wave))
(paint-hires (rotate-180 wave))
(paint-hires (rotate-270 wave))
|#

#|
(paint einstein)
(paint (flip-horiz2 einstein))
(paint (rotate-180 einstein))
(paint (rotate-270 einstein))
|#


;; 2.51
(define (below2 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter2
            painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))
          (paint-top
           (transform-painter2
            painter1
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))
           ))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(paint (below2 einstein einstein))

(define (rotate-90 painter)
  (transform-painter2 painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (below3 painter1 painter2)
  (rotate-270 (beside (rotate-90 painter1) (rotate-90 painter2))))

(paint (below3 einstein einstein))


;; 2.52
;; a

(define wave-smile
  (segments->painter
   (list (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
         (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
         (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
         (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
         (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
         (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
         (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
         (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
         (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
         (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
         (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
         (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
         (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
         (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0))

         ;;eyes
         (make-segment (make-vect 0.4 0.8) (make-vect 0.45 0.8))
         (make-segment (make-vect 0.55 0.8) (make-vect 0.6 0.8))

         ;; mouth
         (make-segment (make-vect 0.43 0.7) (make-vect 0.58 0.7))
         (make-segment (make-vect 0.4 0.72) (make-vect 0.43 0.7))
         (make-segment (make-vect 0.58 0.7) (make-vect 0.6 0.72))
         )))

(paint-hires wave-smile)


;; b
(define (corner-split-2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split-2 painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint-hires (corner-split-2 wave-smile 2))

;; c
(define (square-limit3 painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

(paint-hires (square-limit3 wave-smile 4))
