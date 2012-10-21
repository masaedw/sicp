(add-load-path ".")
(use sicp.ddp)
(use gauche.test)

(test-start "2.84")

;; 問
;;
;; 問題2.83 の raise 演算を使って apply-generic 手続を修正し、本節で論
;; じたように順二に集ていく方法で、引数を同じ型になるまで、強制変換する
;; ようにせよ。二つの型のいずれが塔の中で高いかをテストする方法を考えな
;; ければならない。これをシステムの他の部分と「整合している」方法で行い、
;; 塔に新レベルを追加する時の問題を生じないようにせよ。
;;
;; 解
;;
;; 「整合している」は「加法的に」新レベルを追加できるようにしておけとい
;; う意味であると解釈する。
;;
;; パッケージ定義の中で (inherit child-type parent-type) と書くと、
;; raiseで上がる先の型を定義できるようにする。
;; 新レベルを追加するときには正しくinheritすれば良いだけ。
;; 

(test* "ancestors of scheme-number"
       '(rational complex)
       (ancestors 'scheme-number))

(test* "which is higher type of scheme-number and rational is rational"
       #t
       (higher 'scheme-number 'rational))

(test* "which is higher type of scheme-number and complex is complex"
       #t
       (higher 'scheme-number 'complex))

;; 2.82 2.83 がいままで通りに動いていれば正しく実装ができているはず
(load "2.82")
(load "2.83")
