(load "./2.70.scm")

(define sign5
  '((A 1) (B 2) (C 4) (D 8) (E 16)))

(define sign10
  '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))

(define (main args)
  (let ((t5 (generate-huffman-tree sign5))
        (t10 (generate-huffman-tree sign10)))
    (print "sign5")
    (print (encode '(A) t5))
    (print (encode '(B) t5))
    (print (encode '(C) t5))
    (print (encode '(D) t5))
    (print (encode '(E) t5))
    (print "sign10")
    (print (encode '(A) t10))
    (print (encode '(B) t10))
    (print (encode '(C) t10))
    (print (encode '(D) t10))
    (print (encode '(E) t10))
    (print (encode '(F) t10))
    (print (encode '(G) t10))
    (print (encode '(H) t10))
    (print (encode '(I) t10))
    (print (encode '(J) t10))))

;; n 個の記号の出現頻度を 1, 2, ... , n-1 とした場合、
;; 最高頻度の符号を表すのに必要なビット数は 1 であり、
;; 最低頻度の符号を表すのに必要なビット数は n である。