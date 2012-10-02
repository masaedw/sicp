(add-load-path ".")
(use SICP.DDP)

(define employee-file1 (attach-tag 'file1 (make-hash-table)))

(define employee-file2 (attach-tag 'file2 '()))

;; a.
;; 従業員ファイルは型情報をもっているべき。
;; また、型情報は集合の実装方法を示すものであるべきである。
;;
;; 集合の実装毎に検索用の関数を定義し、演算の表に登録しておく。
;; 従業員ファイルの type-tag を用いて集合の検索用の演算を get することで、
;; 従業員ファイルの構造によらず検索を行うことが出来る。
(define (get-record employee-name employee-file)
  (let ((get-record-1 (get-method 'record (type-tag employee-file))))
    (get-record-1 employee-name employee-file)))

;; b.
;; 従業員レコードには、事業所毎の型情報を付加すべき。
;; その上で、それぞれの型毎に salary 取得の手続きを演算の表に登録しておく。
(define (get-salary employee-record)
  (let ((salary (get-method 'salary (type-tag employee-record))))
    (salary employee-record)))

;; c.
;; get-record は、record が見つからなかった場合に false を返すものとする。
(define (find-employee-record employee-name employee-file-list)
  (if ((null? employee-file-list)
       #f)
      (let ((reocrd (get-record employee-name (car employee-file-list))))
        (if record
            record
            (find-employee-record employee-name (cdr employee-file-list))))))

;; d.
;; 既存のコードに対する変更はない。
;; 新しい事業所のファイルの構造に対する手続きを演算の表に登録すればよい。