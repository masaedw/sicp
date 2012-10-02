(add-load-path ".")
(use SICP)

(define (count-leaves t)
  (accumulate (lambda (x y) (if (null? x) y (+ x y)))
              0
              (map (lambda (x)
                     (if (pair? x)
                         (count-leaves x)
                         1))
                   t)))

(define (main args)
  (p (count-leaves '(1 2 (2 4 5) 2 ((3 5) 1)))))