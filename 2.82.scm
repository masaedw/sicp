(add-load-path ".")
(use sicp.ddp)

(define (main args)

  ;; 2.82
  ;; apply-generic が十分に一般的でない例
  (put-method 'add '(scheme-number scheme-number scheme-number)
              (^(x y z) (print "method1")))
  (put-method 'add '(rational rational scheme-number)
              (^(x y z) (print "method2")))
  (put-method 'add '(rational rational rational)
              (^(x y z) (print "method3")))
  (put-method 'add '(complex rational scheme-number)
              (^(x y z) (print "method4")))

  (apply-generic 'add 5 5 5)
  ;; => method1
  (apply-generic 'add (make-rational 5 1) (make-rational 5 1) 5)
  ;; => method2
  (apply-generic 'add (make-rational 5 1) 5 5)
  ;; => methdo3
  (apply-generic 'add (make-complex-from-real-imag 5 0) (make-rational 5 1) 5)
  ;; => method4
  (apply-generic 'add (make-complex-from-real-imag 5 0) 5 5)
  ;; => error
  ;;
  ;; scheme-number を rational に強制型変換することができるのだから、こ
  ;; れは実行できてほしいが、できない。
  )
