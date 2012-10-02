(define (main args)
  (print (car (cdaddr '(1 3 (5 7) 9))))
  (print (caar '((7))))
  (print (cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))))