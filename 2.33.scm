(add-load-path ".")
(use SICP)

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y)
                (if (null? x) y (+ y 1)))
              0
              sequence))

(define (main args)
  (define l '(1 2 3 4 5))
  (p (accumulate + 0 l)
     (map square l)
     (length l)
     (append l '(6 7 8 9 10))))
