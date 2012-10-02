(define tolerance 0.00001)

(define (printing-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (print next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (main args)
  (printing-fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2))
                        2.0))