(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate-iter combiner (combiner null-value (term a)) term (next a) next b)))

(define (cont-frac-rec n d k)
  (define (combiner i cf)
    (/ (n i) (+ (d i) cf)))
  (define (id x) x)
  (define (inc x) (+ x 1))
  (accumulate-rec combiner (/ (n k) (d k)) id 1 inc (- k 1)))

(define (cont-frac-iter n d k)
  (define (combiner cf i)
    (/ (n i) (+ (d i) cf)))
  (define (id x) (+ (- k x) 1))
  (define (inc x) (+ x 1))
  (accumulate-iter combiner (/ (n k) (d k)) id 1 inc (- k 1)))

(define cont-frac cont-frac-iter)

(define (main args)
  (print (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    1000)))