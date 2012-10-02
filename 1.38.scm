(load "./1.37.scm")

(define (exp-cv)
  (define (d i)
    (if (= (modulo (- i 2) 3) 0)
        (* (+ (/ (- i 2) 3) 1) 2)
        1.0))
  (+ (cont-frac (lambda (i) 1.0) d 10) 2))

(define (main args)
  (print (exp-cf)))
