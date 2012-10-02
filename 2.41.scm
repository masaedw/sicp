(add-load-path ".")
(use SICP)

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-ternaries n)
  (flatmap (lambda (i)
             (map (lambda (j) (cons i j))
                  (unique-pairs (- i 1))))
           (enumerate-interval 2 n)))

(define (=sum? list n)
  (= (accumulate + 0 list) n))

(define (sum-confined-ternaries s n)
  (filter (lambda (list) (=sum? list s))
          (unique-ternaries n)))

(define (main args)
  (p (sum-confined-ternaries 15 20)))