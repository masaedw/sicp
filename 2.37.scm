(add-load-path ".")
(use SICP)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(define (valid-matrix? m)
  (apply = (map length m)))

(define (matrix-rows m)
  (length m))

(define (matrix-cols m)
  (length (car m)))

(define (matrix-*-matrix m n)
  (define (calc m n)
    (let ((cols (transpose n)))
      (map (lambda (u) (matrix-*-vector cols u)) m)))
  (if (and (valid-matrix? m)
           (valid-matrix? n)
           (= (matrix-cols m)
              (matrix-rows n))
           (= (matrix-cols n)
              (matrix-rows m)))
      (calc m n)
      (error "Can't calculate")))

(define (main args)
  (p (matrix-*-matrix '((1 2 3 4) (5 6 7 8)) '((1 2) (3 4) (5 6) (7 8)))))

