(add-load-path ".")
(use SICP)

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (main args)
  (p (list->tree '(1 3 5 7 9 11))))

;; a.
;; partial-tree は、 リスト elts の先頭 n 要素を、釣り合っている木にする。
;;     5
;;    /  \
;;   1     9
;;    \   / \
;;     3 7   11

;; b.
;; list->tree が n 個の要素のリストを変換するのに必要なステップ数の増加
;; の程度は、O(n)
;; 
;; あってる？