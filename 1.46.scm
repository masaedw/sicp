(load "./misc.scm")

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (iterative-improve enough? improve)
  (define (try guess)
    (let ((next (improve guess)))
      (if (enough? guess next)
          next
          (try next))))
  (lambda (first-guess) (try first-guess)))

(define tolerance 0.00001)

(define (close-enough? guess x)
  (< (abs (- guess x)) tolerance))

(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))

(define (sqrt x)
  (define improve (average-damp (lambda (x) (/ 2 x))))
  ((iterative-improve close-enough? improve) 1.0))

(define (main args)
  (print (sqrt 4))
  (print (fixed-point cos 1.0)))
