(define (square x) (* x x))

(define (square-tree1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree)))
       tree))

(define (main argv)
  (map print (list (square-tree1 '(1 (5 2) 3 2))
                   (square-tree2 '(1 (5 2) 3 2)))))