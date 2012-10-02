(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (+-lambda-number lhs rhs)
  (lambda (f) (lambda (x) ((lhs f) ((rhs f) x)))))

(define (inc x) (+ x 1))

(define (dump-lambda-number n)
  (print ((n inc) 0)))

(define (main args)
  (let ((three (+-lambda-number one two)))
    (dump-lambda-number (+-lambda-number three two))))