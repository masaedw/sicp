(add-load-path ".")
(use sicp)

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j) (cons i j))
                  (unique-pairs (- i 1))))
           (enumerate-interval 2 n)))

(define (=sum? ns n)
  (= (accumulate + 0 ns) n))

(define (sum-is-s-triples s n)
  (filter (lambda (ns) (=sum? ns s))
          (unique-triples n)))

(define (main args)
  (p (sum-is-s-triples 10 20)))
