(load "./misc.scm")

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define dx 0.00001)
;;(define dx machine-epsilon)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                           average-damp
                           1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(define (main args)
  (print (newtons-method (cubic 1 -3 -6) 1)))