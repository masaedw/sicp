(define (square x) (* x x))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-ter b
                (- couter 1)
                (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expt2 b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b counter product)
  (if (= counter 0)
      product
      (if (even? counter)
          (fast-expt-iter (square b) (/ counter 2) product)
          (fast-expt-iter b (- counter 1) (* b product)))))
