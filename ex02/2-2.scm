(use gauche.test)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))


(define (length items)
(define (length-iter a count)
  (if (null? a)
      count
      (length-iter (cdr a) (+ 1 count ))))
(length-iter items 0))

;; 2-19
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))


(last-pair (list 23 72 149 34))

(test-section "last-pair")

(test* "" '(1) (last-pair '(1)))
(test* "" '(3) (last-pair '(1 2 3)))


;; 2-18
(define (my-reverse items)
  (if (null? (cdr items))
      items
      (append (my-reverse (cdr items)) (list (car items)))))

(test-section "my-reverse")
(test* "" '(1) (my-reverse '(1)))
(test* "" '(2 1) (my-reverse '(1 2)))
(test* "" '(3 2 1) (my-reverse '(1 2 3)))
(test* "" '(4 3 2 1) (my-reverse '(1 2 3 4)))
(test* "" '(5 4 3 2 1) (my-reverse '(1 2 3 4 5)))


;; 2-19
(define jp-coins (list 500 100 50 10 5 1))
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(cc 10 jp-coins)


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))


;; 2-20
(define (same-parity head . nums)
  (define (evens ns)
    (cond ((null? ns) '())
          ((even? (car ns)) (cons (car ns) (evens (cdr ns))))
          (else (evens (cdr ns)))))
  (define (odds ns)
    (cond ((null? ns) '())
          ((odd? (car ns)) (cons (car ns) (odds (cdr ns))))
          (else (odds (cdr ns)))))
  (if (even? head)
      (cons head (evens nums))
      (cons head (odds nums))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity 2 3 3 3 4 5 6 7)


;;2-21
(define (square-list-1 items)
  (if (null? items)
      '()
      (cons (expt (car items) 2) (square-list-1 (cdr items)))))

(test-section "square-list-1")
(test* "" '(1 4 9) (square-list-1 '(1 2 3)))

(define (square-list-2 items)
  (map (lambda (n) (expt n 2)) items))

(test-section "square-list-2")
(test* "" '(1 4 9) (square-list-2 '(1 2 3)))

;; 2-22
(define (square n)
  (expt n 2))

(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(define (square-list-4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))


;;2-23
(define (my-for-each factor items)
  (if (null? items)
      '()
      (cons (factor (car items))
            (my-for-each factor (cdr items)))
      ))

(my-for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

;;2-25
(define list-1 '(1 3 (5 7) 9))
(define list-2 '((7)))
(define list-3 '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr list-1)))))
(car (car list-2))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-3))))))))))))

;;2-26
(define x (list 1 2 3))
(define y (list 4 5 6))

(test* "" '(1 2 3 4 5 6) (append x y))
(test* "" '((1 2 3) 4 5 6) (cons x y))
(test* "" '((1 2 3) (4 5 6)) (list x y))

;; 2-27
(define x (list (list 1 2) (list 3 4)))
(my-reverse x)

;;(define (deep-reverse items)
;;  (map my-reverse (my-reverse items)))

(define (deep-reverse items)
  (if (not (pair? items))
      items
      (map deep-reverse (my-reverse items))))

(test-section "deep-reverse")
(test* "" '((2 1)) (deep-reverse '((1 2)) ))
(test* "" '((4 3) (2 1)) (deep-reverse '((1 2) (3 4))))
(test* "" '((1 2) ((4 3) (2 1))) (deep-reverse '(((1 2) (3 4)) (2 1))))

(test* "deep-reverse"
 '((4 3) (2 1))
 (deep-reverse '((1 2) (3 4)))
 )
(test* "deep-reverse"
 '((4 (3 2)) 1 )
 (deep-reverse '(1 ((2 3) 4)))
 )

;; 2-28
(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else
         (append (fringe (car tree)) (fringe (cdr tree))))))

(test-section "fringe")
(define x (list (list 1 2) (list 3 4)))
(test* "" '(1 2 3 4) (fringe x))
(test* "" '(1 2 3 4 1 2 3 4) (fringe (list x x)))
(test* "" '(1 2 3 4 5) (fringe '(1 2 (3) 4 5)))
(test* "" '(1 2 3 4 5) (fringe '((1) (2) (3) (4) (5))))
(test* "" '(1 2 3 4 5) (fringe '(1 (2 (3) 4) 5)))
(test* "" '(1 2 3 4 5 6 7 8 9 10) (fringe '(((((((1 2 3 4) 5) 6) 7) 8) 9) 10)))


;;2-29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

;;(define (right-branch mobile)
;;  (cadr mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

;;(define (branch-structure branch)
;;  (cadr branch))

(define (branch-structure branch)
  (cdr branch))

;;(define (total-weight mobile)
;;  (if (not (pair? mobile))
;;      mobile
;;      (+ (total-weight (left-branch mobile))
;;         (total-weight (right-branch mobile)))))

(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define (get-torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (= (get-torque (left-branch mobile))
     (get-torque (right-branch mobile))))


(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))


(test-section "mobile")

(test* "total-weight"
 5
 (total-weight (make-mobile (make-branch 2 2) (make-branch 3 3))))

(test* "total-weight"
       7
       (total-weight (make-mobile
                (make-branch 2
                 (make-mobile
                  (make-branch 1 2)
                  (make-branch 1 2)))
                (make-branch 3 3))))


(test* "get-torque"
       14
       (get-torque (make-branch 2
                                (make-mobile
                                 (make-branch 2
                                              (make-mobile
                                               (make-branch 1 2)
                                               (make-branch 1 2)))
                                 (make-branch 3 3)))))

(test* "get-torque"
       45
       (get-torque (make-branch 3
                                (make-mobile
                                 (make-branch 3
                                              (make-mobile
                                               (make-branch 2 5)
                                               (make-branch 1 5)))
                                 (make-branch 3 5)))))


(test* "balanced?"
       #t
       (balanced?
        (make-mobile
         (make-branch 3
                      (make-mobile
                       (make-branch 3
                                    (make-mobile
                                     (make-branch 2 5)
                                     (make-branch 1 5)))
                       (make-branch 3 5)))
         (make-mobile 9
                      (make-mobile
                       (make-branch 2 2)
                       (make-branch 3 3))))))

(test* "balanced?"
       #f
       (balanced?
        (make-mobile
         (make-branch 3
                      (make-mobile
                       (make-branch 3
                                    (make-mobile
                                     (make-branch 2 5)
                                     (make-branch 1 5)))
                       (make-branch 3 5)))
         (make-mobile 4
                      (make-mobile
                       (make-branch 2 2)
                       (make-branch 3 3))))))

;; 2-30

(define (square x)
  (* x x))

(define (square-tree-1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (square sub-tree)))
       tree))


(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree-3 tree) (tree-map square tree))


(test-section "square-tree")

(test* "square-tree-1"
       '(1 (4 (9 16) 25) (36 49))
       (square-tree-1
        (list 1
              (list 2 (list 3 4) 5)
              (list 6 7))))

(test* "square-tree-2"
       '(1 (4 (9 16) 25) (36 49))
       (square-tree-2
        (list 1
              (list 2 (list 3 4) 5)
              (list 6 7))))

(test* "square-tree-3"
       '(1 (4 (9 16) 25) (36 49))
       (square-tree-3
        (list 1
              (list 2 (list 3 4) 5)
              (list 6 7))))

;;2-32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (x) (cons (car s) x))
                      rest)))))

(test* "subsets"
       '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
       (subsets '(1 2 3))
       )
