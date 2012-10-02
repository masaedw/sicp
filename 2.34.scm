(add-load-path ".")
(use SICP)

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(define (main args)
  (p (horner-eval 2 '(1 3 0 5 0 1))))