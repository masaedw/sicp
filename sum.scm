(define (inc x) (+ x 1))
(define (cube x) (* x x x))

(define (sum-rec term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-rec term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define sum sum-iter)

(define (sum-cubes a b)
  (sum cube a inc b))

(print (sum-cubes 1 10))