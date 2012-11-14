(add-load-path ".")
(use sicp.ddp)
(use gauche.test)

;; 過去の実装を壊してないこと
(load "2.87")

(test-start "2.88")

(test* "negate of 1"
       -1
       (negate 1)
       equ?)

(test* "negate of -1"
       1
       (negate -1)
       equ?)

(test* "negate of (make-rational 5 4)"
       (make-rational -5 4)
       (negate (make-rational 5 4))
       equ?)

(test* "negate of (make-rational -5 4)"
       (make-rational 5 4)
       (negate (make-rational -5 4))
       equ?)

(test* "negate of (make-rational 5 -4)"
       (make-rational 5 4)
       (negate (make-rational 5 -4))
       equ?)

(define (almost-eq? a b)
  (< (abs (- a b)) 1.0e-10))

(define (almost-equ?-complex a b)
  (and (almost-eq? (real-part a)
                   (real-part b))
       (almost-eq? (imag-part a)
                   (imag-part b))))

(test* "negate of (make-complex-from-real-imag 1 0)"
       (make-complex-from-real-imag -1 0)
       (negate (make-complex-from-real-imag 1 0))
       almost-equ?-complex)

