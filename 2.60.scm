(add-load-path ".")
(use SICP)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (if (or (null? set1))
      set2
      (cons (car set1)
            (union-set (cdr set1) set2))))

(define (main args)
  (p (intersection-set '(1 2 3 5 57) '(2 8 9 0 3))
     (union-set '(1 2 3 5 57) '(2 8 9 0 3))))

;; この重複を許す集合を使いたくなる場合は、
;; * 追加の回数が多いとき
;; * 和集合が多いとき
;; って、どんな場合だろう？
