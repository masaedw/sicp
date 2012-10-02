(define x (list (list 1 2) (list 3 4)))

(define (fringe l)
  (cond ((null? l) l)
        ((not (pair? l)) (list l))
        (else (append (fringe (car l))
                      (fringe (cdr l))))))

(define (main args)
  (print (fringe x))
  (print (fringe (list x x))))