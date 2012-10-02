(define tolerance 0.0000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (main args)
  (print (fixed-point (lambda (x) (/ (+ x 1 (/ 1 x)) 2))
                      2.0)))