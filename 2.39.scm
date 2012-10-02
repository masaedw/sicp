(add-load-path ".")
(use SICP)

(define (reverser sequence)
  (fold-right (lambda (x y) (cons x y)) nil sequence))

(define (reversel sequence)
  (fold-left (lambda (x y) (append x (list y))) nil sequence))

(define (main args)
  (p (reverser '(1 2 3 4 5 6))
     (reversel '(1 2 3 4 5 6))))