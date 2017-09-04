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
