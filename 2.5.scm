(define (inc x) (+ x 1))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (num_pair_accessor ab)
  (define (try a n)
    (if (= (modulo n ab) 0)
        (try (inc a) (/ n ab))
        a))
  (lambda (n) (try 0 n)))

(define (car p)
  ((num_pair_accessor 2) p))

(define (cdr p)
  ((num_pair_accessor 3) p))

(define (main args)
  (define hoge (cons 4 5))
  (print hoge)
  (print (car hoge))
  (print (cdr hoge)))