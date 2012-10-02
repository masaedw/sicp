(load "./2.11.scm")

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (/ (* c p) 100)))
    (make-center-width c w)))

(define (percent i)
  (/ (* (width i) 100) (center i)))
