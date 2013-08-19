(add-load-path ".")
(use sicp)

(define (main args)
  (print #`"The smallest divisor of 199 is ,(smallest-divisor 199)")
  (print #`"The smallest divisor of 1999 is ,(smallest-divisor 1999)")
  (print #`"The smallest divisor of 19999 is ,(smallest-divisor 19999)"))
