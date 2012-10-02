(load "./1.37.scm")

(define (cont-frac-inv n d k)
  (define (combiner i r)
    (/ (n i) (- (d i) r)))
  (define (rev i) (+ (- k i) 1))
  (accumulate combiner (/ (n k) (d k)) rev 1 inc k))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (square x)))
  (define (d i)
    (- (* i 2) 1))
  (cont-frac-inv n d k))

(define (square x) (* x x))

(define (main args)
  (print (tan-cf 3 10)))