(load "./misc.scm")

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (repeated (compose f f) (- n 1))))

(define (n-root x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (dec n))))
                            (repeated average-damp (floor (/ (log n) (log 2))))
                            1.0))

(define (n-root-check x n p)
  (n-root (expt x n) n p))

;; ^4 2 2^2
;; ^8 3 2^3
;; ^32 4 2^5

(define (main args)
  ;;(print (n-root-check 2 512 5)))
  (print (n-root 256 8)))
