(add-load-path ".")
(use SICP)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;; 追加
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; 積集合
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; 和集合
(define (union-set set1 set2)
  (cond ((or (null? set1)) set2)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

(define (main args)
  (p (intersection-set '(1 2 3 5 57) '(2 8 9 0 3))
     (union-set '(1 2 3 5 57) '(2 8 9 0 3))))