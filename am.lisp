(defparameter *rules* '(:a :a :a))
(defparameter *a* '(:sub :v))
(defparameter *name* #(:creature :place))
(defparameter *creature* #("猫" "犬" "人間"))
(defparameter *place* #("森" "海" "山"))
(defparameter *sub* '(:name :p))
(defparameter *p* #(("が") ("は") ("に") ("を") ("や" :sub)))
(defparameter *v* #("歩く" ((:none (:place #("を" "で"))) "走る")))

;list -> ルール
;vector -> シンボル(中から1つ選ぶ)

(defmacro choose (vec)
  "配列から1つランダムに取り出す"
  `(svref ,vec (random (length ,vec))))

(defun am (rules)
  "ルールの展開"
  (format nil "~{~A~^~}" (loop for x in rules collect (ext x))))

(defun ext (rule)
  "シンボルの展開"
  (case rule
    ;文章
    (:a (concatenate 'string (am *a*) "。"))
    ;名詞
    (:name (ext (choose *name*)))
    ;名詞-生物
    (:creature (choose *creature*))
    ;名詞-場所
    (:place (choose *place*))
    ;主語
    (:sub (am *sub*))
    ;助詞
    (:p (am (choose *p*)))
    ;動詞
    (:v (ext (choose *v*)))
    ;省略
    (:none "")
    ;その他
    (t (if (typep rule 'string)
         ;ルール
         rule
         (if (typep rule 'vector)
           ;シンボル
           (choose rule)
           ;文字列
           (am rule))))))

