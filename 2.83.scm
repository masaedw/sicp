(add-load-path ".")
(use sicp.ddp)
(use gauche.test)

(test-start "2.83")

(test* "raise of scheme-number"
       (make-rational 5 1)
       (raise 5)
       equ?)

(test* "raise of rational"
       (make-complex-from-real-imag 5 0)
       (raise (make-rational 5 1))
       equ?)
