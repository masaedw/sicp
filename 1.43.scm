(load "./misc.scm")

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (main args)
  (print ((repeated inc 6) 5)))