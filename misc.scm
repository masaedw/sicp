(define (id x) x)
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average a b) (/ (+ a b) 2))

(define (find-machine-epsilon i)
  (let ((eps (/ i 2)))
    (if (= (+ eps 1) 1)
        i
        (find-machine-epsilon eps))))

(define machine-epsilon 2.220446049250313e-16)
;;(find-machine-epsilon 1.0))


;;(define tolerance machine-epsilon)
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))(load "./accumulate.scm")


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
