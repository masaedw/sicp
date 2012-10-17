(add-load-path ".")
(use sicp.ddp)
(use gauche.test)


;; すべての引数を第一引数の型、次に第二引数の型等々に強制変換を試みる実装
;; これは十分に一般的ではない。
;;
;; (put-method 'add '(complex rational scheme-number) ...)
;; という3引数のメソッドが存在した場合、
;; (apply-generic 'add (make-complex-from-real-imag 5 0) (make-rational 5 1) (make-scheme-number 5))
;; は実行可能だが、
;; (apply-generic 'add (make-complex-from-real-imag 5 0) (make-scheme-number 5) (make-scheme-number 5))
;; は(scheme-number->crationalが存在するにもかかわらず)実行できない。
;; 与えられた引数を全て単一の型に強制変換しようとするからである。

;; 2.82
;; apply-generic が十分に一般的でない例

(put-method 'add '(scheme-number scheme-number scheme-number)
            (^(x y z) "method1"))
(put-method 'add '(rational rational scheme-number)
            (^(x y z) "method2"))
(put-method 'add '(rational rational rational)
            (^(x y z) "method3"))
(put-method 'add '(complex rational scheme-number)
            (^(x y z) "method4"))

(test* "method1"
       "method1"
       (apply-generic 'add 5 5 5))

(test* "method2"
       "method2"
       (apply-generic 'add (make-rational 5 1) (make-rational 5 1) 5))

(test* "method3"
       "method3"
       (apply-generic 'add (make-rational 5 1) 5 5))

(test* "method4"
       "method4"
       (apply-generic 'add (make-complex-from-real-imag 5 0) (make-rational 5 1) 5))

;; scheme-number を rational に強制型変換することができるのだから、こ
;; れは実行できてほしいのだが、できない。
(test* "error"
       (test-error)
       (apply-generic 'add (make-complex-from-real-imag 5 0) 5 5))
