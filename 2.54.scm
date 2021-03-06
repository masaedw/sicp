(add-load-path ".")
(use SICP)

(define (equal?-o a b)
  (if (and (pair? a) (pair? b))
      (and (equal?-o (car a) (car b))
           (equal?-o (cdr a) (cdr b)))
      (eq? a b)))

(define (main args)
  (define a '(this is a list))
  (define b '(this (is a) list))
  (p (equal? a a)
     (equal? a b)
     (equal?-o a a)
     (equal?-o a b)))