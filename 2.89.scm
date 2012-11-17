(add-load-path ".")
(use sicp.ddp)
(use gauche.test)

;; 過去の実装を壊してないこと
(load "2.88")

(test-start "2.89")

(test* "add (make-polynomial-dense 'x '(5 3 0 5 1)) (make-polynomial-dense 'x '(1 2 3))"
       (make-polynomial-dense 'x '(5 3 1 7 4))
       (add (make-polynomial-dense 'x '(5 3 0 5 1)) (make-polynomial-dense 'x '(1 2 3)))
       ) ;; equ? はこの段階では未定義

(test* "negate (make-polynomial-dense 'x '(5 3 0 5 1))"
       (make-polynomial-dense 'x '(-5 -3 0 -5 -1))
       (negate (make-polynomial-dense 'x '(5 3 0 5 1)))
       ) ;; equ? はこの段階では未定義

(test* "sub (make-polynomial-dense 'x '(5 3 0 5 1)) (make-polynomial-dense 'x '(5 3 1 3 1))"
       (make-polynomial-dense 'x '(0 0 -1 2 0))
       (sub (make-polynomial-dense 'x '(5 3 0 5 1)) (make-polynomial-dense 'x '(5 3 1 3 1)))
       ) ;; equ? はこの段階では未定義

(test* "mul (make-polynomial-dense 'x '(5 1)) (make-polynomial-dense 'x '(1 2 3))"
       (make-polynomial-dense 'x '(5 11 17 3))
       (mul (make-polynomial-dense 'x '(5 1)) (make-polynomial-dense 'x '(1 2 3)))
       ) ;; equ? はこの段階では未定義

(test* "=zero? (make-polynomial-dense 'x '(0 0 0))"
       #t
       (=zero? (make-polynomial-dense 'x '(0 0 0)))
       )
