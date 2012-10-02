(load "./misc.scm")

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (repeated (compose f f) (- n 1))))

(define (average3 a b c)
  (/ (+ a b c) 3))

(define (smooth f)
  (lambda (x)
    (average3 (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))
