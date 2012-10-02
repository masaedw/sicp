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
