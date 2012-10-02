(load "./misc.scm")

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (main args)
  (print ((compose square inc) 6)))