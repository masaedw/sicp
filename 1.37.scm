(load "./misc.scm")

(define (cont-frac n d k)
  (define (combiner i r)
    (/ (n i) (+ (d i) r)))
  (define (rev i) (+ (- k i) 1))
  (accumulate combiner (/ (n k) (d k)) rev 1 inc k))

(define (main args)
  (print (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    11)))