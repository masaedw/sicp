;; http://oss.timedia.co.jp/index.fcgi/kahua-web/show/SICP/ex-2.13
;; x の相対許容誤差を p[x]、y の相対許容誤差を p[y] とすると
;;
;; (x ± p[x]x)・(y ± p[y]y) → xy ± (p[x] ＋ p[y] ＋ p[x]p[y])xy
;;
;; ここで、p[x] および p[y] が小さいとすると
;; p[x]p[y] の項は無視することができるので 2 つの区間の積は
;;
;; xy ± (p[x] ＋ p[y])xy
;;
;; で近似できる。
;; したがって 2 つの区間の積の相対許容誤差は 2 つの区間
;; の相対許容誤差の和で近似できる。

