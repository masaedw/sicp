(load "./2.68.scm")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (let ((pri (car pairs))
            (sec (cadr pairs)))
        (successive-merge (adjoin-set (make-code-tree pri sec) (cddr pairs))))))

(define sample-pairs
  '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

(define (main args)
  (p (generate-huffman-tree sample-pairs)))