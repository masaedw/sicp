(add-load-path ".")
(use sicp.ddp)
(use gauche.test)

;; 2.84 がいままで通りに動いていれば正しく実装ができているはず
(load "2.84")

(test-start "2.85")

;; 問
;;
;;  apply-generic 手続を修正し、答を単純化するようにせよ
;;
;; 解
;;
;; 1. ヒントの通りにまず一段押し下げるジェネリック関数projectを定義する。
;;    projectされた値は元の値と数値的に等しいとは限らない。
;;
;; 2. ヒントの通り可能な限り塔を下るdrop手続を書く。
;;    dropされたものは元の値と数値的に等しい。
;;
;; 3. apply-generic を drop を使うように修正する
;;    → これは……やらない方がいいのでは？
;;       apply-generic は generic procedure を組み立てる仕組みであって、
;;       返り値をいじくったりしない方がいいと思う。booleanを返す関数もあるし。
;;       というかそういう場合にどうすればいいのかわからない。

(test* "project of 5+0i is rational 5/1"
       (make-rational 5 1)
       (project (make-complex-from-real-imag 5 0))
       equ?)

(test* "project of 5+1i is rational 5/1"
       (make-rational 5 1)
       (project (make-complex-from-real-imag 5 1))
       equ?)

(test* "project of 5/2 is scheme-number 2"
       2
       (project (make-rational 5 2))
       equ?)

(test* "drop of 5+0i is scheme-number 5"
       5
       (drop-tower (make-complex-from-real-imag 5 0))
       equ?)
