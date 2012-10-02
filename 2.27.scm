(define (reverse list)
  (define (sub src dst)
    (if (null? src)
        dst
        (sub (cdr src) (cons (car src) dst))))
  (sub list '()))

(define (deep-reverse list)
  (define (sub src dst)
    (if (null? src)
        dst
        (sub (cdr src) (cons (if (pair? (car src))
                                 (deep-reverse (car src))
                                 (car src))
                             dst))))
  (sub list '()))

(define (main args)
  (print (deep-reverse '(1 2 3 4 5 6 7)))
  (print (deep-reverse '(1 (2 3 4) 5 6 7)))
  (print (deep-reverse '(1 (2 3) 4 (5 6) 7))))