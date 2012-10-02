(add-load-path ".")
(use SICP)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (main args)
  (p (fold-right / 1 (list 1 2 3))
     (fold-left / 1 (list 1 2 3))
     (fold-right list nil (list 1 2 3))
     (fold-left list nil (list 1 2 3))))

;; fold-right と fold-left が同じ結果になるために op が満たすべき性質
;; (op a b) == (op b a) となること