(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))))

(define (g n)
  (h 2 1 0 n))

(define (h a b c n)
  (cond ((= n 0) c)
        (else (h (+ a b c) a b (- n 1)))))
