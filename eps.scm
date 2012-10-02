(define (find-machine-epsilon i)
  (let ((eps (/ i 2)))
    (if (= (+ eps 1) 1)
        i
        (find-machine-epsilon eps))))

(define machine-epsilon 2.220446049250313e-16)
;  (find-machine-epsilon 1.0))

(define (main args)
  (print machine-epsilon))