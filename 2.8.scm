(load "./2.7.scm")

;; 差の最小値は引かれる区間の最小値と引く区間の最大値との差であり、
;; 差の最大値は引かれる区間の最大値と引く区間の最小値との差である。
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
