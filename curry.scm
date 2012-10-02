(define (curry proc x)
  (lambda (arg) (proc x arg)))

(define (main args)
  (print ((curry + 10) 100)))