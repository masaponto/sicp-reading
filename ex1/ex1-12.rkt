(define (pas n m)
  (cond ((or (= n m) (= m 1)) 1)
        (else (+ (pas (- n 1) (- m 1)) (pas (- n 1) m)))))
