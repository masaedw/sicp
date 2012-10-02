(define (inc x) (+ x 1))
(define (cube x) (* x x x))
(define (id x) x)

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (conviner (term a)
                (product-rec term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define accumulate accumulate-iter)

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (main args)
  (print (sum id 1 inc 10))
  (print (product id 1 inc 10)))