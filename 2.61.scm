(add-load-path ".")
(use SICP)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((< x (car set))
         (cons x set))
        ((= x (car set))
         set)
        (else
         (cons (car set) (adjoin-set x (cdr set))))))

;; 積集合
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (main args)
  (p (adjoin-set 3 '(1 2 4 6))
     (intersection-set '(1 2 3 5 57) '(0 2 3 8 9))))