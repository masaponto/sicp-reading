(use gauche.test)

;; 2.53
(test-section "2.53")

(test* "" '(a b c) (list 'a 'b 'c ))
(test* "" '((george)) (list (list 'george)))
(test* "" '((y1 y2)) (cdr '((x1 x2) (y1 y2))))
(test* "" '(y1 y2) (cadr '((x1 x2) (y1 y2))))
(test* "" #f (pair? (car '(a short list))))
(test* "" #f (memq 'red '((red shoes) (blue socks))))
(test* "" '(red shoes blue socks) (memq 'red '(red shoes blue socks)))

;; 2.54
(define (equal-? x y)
  (cond
   ((and (null? x) (null? y)) #t)
   ((and (null? x) (not (null? y))) #f)
   ((and (not (null? x)) (null? y)) #f)

   ((and (pair? x) (not (pair? y))) #f)
   ((and (not (pair? x)) (pair? y)) #f)

   ((and (not (pair? x)) (not (pair? y)))
    (eq? x y))

   ((and (pair? x) (pair? y))
    (and (equal-? (car x) (car y))
         (equal-? (cdr x) (cdr y))))
   ))

(test-section "2.54")
(test* "" (equal? '() '()) (equal-? '() '()))
(test* "" (equal? '(a) '()) (equal-? '(a) '()))
(test* "" (equal? '() '(a)) (equal-? '() '(a)))
(test* "" (equal? '(a) '(a)) (equal-? '(a) '(a)))

(test* "" (equal? '(a b) '(a)) (equal-? '(a b) '(a)))
(test* "" (equal? '(a b) '(a b)) (equal-? '(a b) '(a b)))

(test* "" (equal? '(a b (a)) '(a b a)) (equal-? '(a b (a)) '(a b a)))

(test* "" (equal? '(a b (a b)) '(a b (a b)))
       (equal-? '(a b (a b)) '(a b (a b))))

(test* "" (equal? '(((a))) '((a))) (equal-? '(((a))) '((a))))
(test* "" (equal? '((a)) '((a))) (equal-? '((a)) '((a))))


;;2.3.4
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))

        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (- (exponent exp) 1)))
          (deriv (base exp) var)))

        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;(define (make-sum a1 a2) (list '+ a1 a2))
;;(define (make-product m1 m2) (list '* m1 m2))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

;; 2-56
(define (exponentiation? x) (and (pair? x)
                                 (eq? (car x) '**)))
(define (base p) (cadr p))

(define (exponent p) (caddr p))

(define (make-exponentiation a1 a2)
  (cond ((=number? a2 0) 1)
        ((=number? a2 1) a1)
        (else (list '** a1 a2))))

(test-section "ex 2.56")

(test* "make-exponentiation"
       (make-exponentiation 0 0)
       1)

(test* "make-exponentiation"
       (make-exponentiation 0 1)
       0)

(test* "make-exponentiation"
       (make-exponentiation 1 0)
       1)

(test* "make-exponentiation"
       (make-exponentiation 1 1)
       1)

(test* "make-exponentiation"
       'x
       (make-exponentiation 'x 1))

(test* "deriv"
       '(* 3 (* 2 x))
       (deriv '(* 3 (** x 2)) 'x))

;; 2.57
(define (augend s)
  (let ((x (cddr s)))
    (cond ((or (pair? (car x))
               (null? (cdr x))) (car x))
           (else (append '(+) x)))))

(test* "augend"
       '(+ 3 4)
       (augend '(+ 2 3 4)))

(test* "augend"
       '3
       (augend '(+ 2 3)))

(test* "augend"
       '(* 2 3)
       (augend '(+ 2 (* 2 3))))


(define (multiplicand s)
  (let ((x (cddr s)))
    (cond ((or (pair? (car x))
               (null? (cdr x))) (car x))
          (else (append '(*) x)))))

(define (exponent s)
  (let ((x (cddr s)))
    (cond ((or (pair? (car x))
               (null? (cdr x))) (car x))
          (else (append '(**) x)))))


(test* "multiplier"
       '(* 3 4)
       (multiplier '(* 2 3 4)))

(test* "multiplier"
       '3
       (multiplier '(* 2 3)))

(test* "deriv"
       '(* 3 (* 2 x))
       (deriv '(* 3 (** x 2)) 'x))


(test-section "ex 2.57")

(test* "augend"
       '(+ 1 x)
       (augend '(+ 1 1 x)))

(test* "multiplicand"
       '(* 1 x)
       (multiplicand '(* 1 1 x)))

(test* "exponent"
       '(** x y)
       (exponent '(** 3 x y)))

(test* "deriv"
       '(+ (* x y) (* y (+ x 3)))
       (deriv '(* x y (+ x 3)) 'x))

;; 2.58 a

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x) (and (pair? x)
                                 (eq? (cadr x) '**)))
(define (base p) (car p))

(define (exponent p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation a1 a2)
  (cond ((=number? a2 0) 1)
        ((=number? a2 1) a1)
        (else (list a1 '** a2))))

(test-section "ex 2.58 - a")

(test* "sum?" #t (sum? '(1 + 2)))
(test* "sum?" #f (sum? '(+ 1 2)))

(test* "addend" 1 (addend '(1 + 2)))

(test* "augend" 2 (augend '(1 + 2)))

(test* "make-sum" '(1 + x) (make-sum 1 'x))
(test* "make-sum" 2 (make-sum 1 1))
(test* "make-sum" 1 (make-sum 1 0))
(test* "make-sum" '(x + y) (make-sum 'x 'y))

(test* "product?" #f (product? '(* 1 2)))

(test* "multiplier" 1 (multiplier '(1 * x)))

(test* "multiplicand" 'x
       (multiplicand '(1 * x)))

(test* "make-sum" '(2 * x) (make-product 2 'x))
(test* "make-sum" 1 (make-product 1 1))
(test* "make-sum" 0 (make-product 1 0))
(test* "make-sum" '(x * y) (make-product 'x 'y))

(test* "deriv" 1
       (deriv '(1 + x) 'x))

(test* "deriv" 'y
       (deriv '(x * y) 'x))

(test* "deriv" '(3 + y)
       (deriv '((3 * x) + (y * x)) 'x))

(test* "deriv" 'x
       (deriv '((3 * x) + (y * x)) 'y))

(test* "deriv" '1
       (deriv '(1 * x + y * 1) 'x))

;; 2.58 b

(define (sum? s)
  (bin-expr-of? '+ s))

(define (product? s)
  (bin-expr-of? '* s))

(define (bin-expr-of? op expr)
  (and
   (pair? expr)
   (pair? (cdr expr))
   (or
    (eq? (cadr expr) op)
    (bin-expr-of? op (cddr expr)))))

(define (addend exp)
  (define (iter op rest left)
    (cond
     ((null? rest)
      (error "operator not found: " op " in " (reverse left)))
     ((eq? (car rest) op)
      (left))
     (else (iter
            op
            (append rest (car rest))
            (cdr rest)))))
  (iter '+ (cdr exp) (car exp)))


;;(define (augend s)
;;  (if (= (length s) 3)
;;      (caddr s)
;;      (cddr s)))
;;
;;(define (multiplicand p)
;;  (if (= (length p) 3)
;;      (caddr p)
;;      (cddr p)))
;;

(test* "deriv" 1
       (deriv '(1 + x) 'x))

(test* "deriv" 'y
       (deriv '(x * y) 'x))

(test* "deriv" '(3 + y)
       (deriv '((3 * x) + (y * x)) 'x))

(test* "deriv" 'x
       (deriv '((3 * x) + (y * x)) 'y))

(test* "deriv" '0
       (deriv '(1 + 1 + 1) 'x))

(test* "deriv" '1
       (deriv '(1 * x + y * 1) 'x))

(test* "deriv" '4
       (deriv '(x + 3 * (x + y + 2)) 'x))
