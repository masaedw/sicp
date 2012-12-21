(add-load-path ".")
(use sicp.ddp)
(use gauche.test)

;; 過去の実装を壊してないこと
(load "2.88")

(test-start "2.90")

;; 問題を読むと、complexでしたようにやりなさいと書いてある。これはつま
;; りsparseとdenseで共通の抽象的な手続を実装し、これを用いて数値演算を
;; 実装するということである。
;;
;; complexの場合は極座標形式と複素平面形式の表現があり、どちらの表現の
;; 値であっても関係なく、極座標として扱うことも、複素平面として扱うこと
;; もできた。
;;
;; 今回も同様に考えると、まず濃いデータ形式と薄いデータ形式のパッケージ
;; を用意し、次に両方に使える濃い項リストと薄い項リストを取得する手続を
;; 実装し、最終的にこれを使って数値演算を実装する形になるであろう。数値
;; 演算を実装する際にどの演算にどの形式を使うべきかについては、加減算は
;; リストをマージするだけで良いので濃い形式、積算については0の演算を省
;; 略できるため薄い形式が適していると考えられる。


;; sparse type

(define make-sparse (get-method 'make-from-term-list 'sparse))
(define make-dense (get-method 'make-from-coeff-list 'dense))

(test* "(make-from-term-list 'x '((5 2) (3 1) (0 4)))"
       '(sparse x (5 2) (3 1) (0 4))
       (make-sparse 'x '((5 2) (3 1) (0 4)))
       )

(test* "variable of (sparse x (5 2) (3 1) (0 4))"
       'x
       (variable (make-sparse 'x '((5 2) (3 1) (0 4))))
       )

(test* "term-list of (sparse x (5 2) (3 1) (0 4))"
       '((5 2) (3 1) (0 4))
       (term-list (make-sparse 'x '((5 2) (3 1) (0 4))))
       )

(test* "coeff-list of (sparse x (5 2) (3 1) (0 4))"
       '(2 0 1 0 0 4)
       (coeff-list (make-sparse 'x '((5 2) (3 1) (0 4))))
       )

(test* "negate of (sparse x (5 2) (3 1) (0 4))"
       (make-sparse 'x '((5 -2) (3 -1) (0 -4)))
       (negate (make-sparse 'x '((5 2) (3 1) (0 4))))
       )

(test* "=zero? of (sparse x (5 2) (3 1) (0 4))"
       #f
       (=zero? (make-sparse 'x '((5 2) (3 1) (0 4))))
       )

(test* "=zero? of (sparse x (5 0) (3 0) (0 0))"
       #t
       (=zero? (make-sparse 'x '((5 0) (3 0) (0 0))))
       )

(test* "=zero? of (sparse x)"
       #t
       (=zero? (make-sparse 'x '()))
       )

;; dense type

(test* "(make-from-coeffs 'x '(2 0 1 0 0 4))"
       '(dense x 2 0 1 0 0 4)
       (make-dense 'x '(2 0 1 0 0 4))
       )

(test* "variable of (dense x 2 0 1 0 0 4)"
       'x
       (variable (make-dense 'x '(2 0 1 0 0 4)))
       )

(test* "term-list of (dense x 2 0 1 0 0 4)"
       '((5 2) (3 1) (0 4))
       (term-list (make-dense 'x '(2 0 1 0 0 4)))
       )

(test* "coeff-list of (dense x 2 0 1 0 0 4)"
       '(2 0 1 0 0 4)
       (coeff-list (make-dense 'x '(2 0 1 0 0 4)))
       )

(test* "negate of (dense x 2 0 1 0 0 4)"
       (make-dense 'x '(-2 0 -1 0 0 -4))
       (negate (make-dense 'x '(2 0 1 0 0 4)))
       )

(test* "=zero? of (dense x 2 0 1 0 0 4)"
       #f
       (=zero? (make-dense 'x '(2 0 1 0 0 4)))
       )

(test* "=zero? of (dense x 0 0 0 0 0)"
       #t
       (=zero? (make-dense 'x '(0 0 0 0 0)))
       )

(test* "=zero? of (dense x)"
       #t
       (=zero? (make-dense 'x '()))
       )

;; polynomial package

(test* "(add (dense x 1 2 3 4 5) (sparse x (4 1) (2 3)))"
       (make-from-coeff-list 'x '(2 2 6 4 5))
       (add (make-from-coeff-list 'x '(1 2 3 4 5))
            (make-from-term-list 'x '((4 1) (2 3))))
       equ?)

(test* "(sub (dense x 1 2 3 4 5) (sparse x (4 1) (2 3)))"
       (make-from-coeff-list 'x '(0 2 0 4 5))
       (sub (make-from-coeff-list 'x '(1 2 3 4 5))
            (make-from-term-list 'x '((4 1) (2 3))))
       equ?)

(test* "(mul (dense x 1 2 3 4 5) (sparse x (4 1) (2 3)))"
       (make-from-term-list 'x '((8 1) (7 2) (6 6) (5 10) (4 14) (3 12) (2 15)))
       (mul (make-from-coeff-list 'x '(1 2 3 4 5))
            (make-from-term-list 'x '((4 1) (2 3))))
       equ?)

(test* "(negate (dense x 1 2 3 4 5))"
       (make-from-coeff-list 'x '(-1 -2 -3 -4 -5))
       (negate (make-from-coeff-list 'x '(1 2 3 4 5)))
       equ?)

(test* "(negate (sparse x (4 1) (2 3)))"
       (make-from-term-list 'x '((4 -1) (2 -3)))
       (negate (make-from-term-list 'x '((4 1) (2 3))))
       equ?)

(test* "(equ? (sparse x (0 1)) (dense x 1))"
       (make-from-term-list 'x '((0 1)))
       (make-from-coeff-list 'x '(1))
       equ?)

(test* "(=zero? (sparse x (0 0)))"
       #t
       (=zero? (make-from-term-list 'x '((0 0)))))

(test* "(=zero? (dense x 0))"
       #t
       (=zero? (make-from-coeff-list 'x '(0))))
