(defvar *abstruct-list* `((:pg . ,*pg*)
                          (:sub . ,*sub*)
                          (:verb . ,*verb*)
                          (:noun . ,*noun*)
                          (:creature . ,*creature*)
                          (:place . ,*place*)
                          (:par . ,*par*)
                          ))

(defvar *pg* '(:sub :verb "。"))
(defvar *sub* '(:noun :par))
(defvar *noun* #(:creature :place))

(defstruct
  (vocab (:constructor defvocab
          (sym parent attr contain rule)))
  (sym "")
  (parent nil)
  (attr #())
  (contain #())
  (rule '()))

(defparameter *verb*
  (defvocab "verb" nil nil
            `#(,(defvocab "移動する" :verb
                        #("足")
                        nil
                        '(:contain)))
            '(:contain)))

;指定した要素を語彙の含む要素から探す
(defun find-vocab (parent sym)
  (loop for v across (vocab-contain parent)
        when (string= (vocab-sym v) sym)
        nconc v))

(defun find-vocab-recursive (parent sym)
  (if (vocab-p parent)
    (if (string= (vocab-sym parent) sym) parent
      (loop for v across (vocab-contain parent)
            collect (find-vocab-recursive v sym)))))

(defun find-all-vocab (sym)
  (loop for ab in *abstruct-list*
        append (find-vocab-recursive ab sym)))

;語彙の含む要素を定義
(defmacro defcontain (parent sym contain)
  `(setf (vocab-contain (car (find-vocab ,parent ,sym))) ,contain))

(defcontain *verb* "移動する"
            #((defvocab "歩く")))

(defparameter *creature*
  (defvocab "creature" nil nil
            `#(,(defvocab "人間" :creature
                        #("足" "腕")
                        #("たかし")
                        '(:contain))
              ,(defvocab "猫" :creature
                        #("足" "手")
                        #("三毛猫")
                        '(:contain))
              ,(defvocab "犬" :creature
                        #("足" "手")
                        #("柴犬" "土佐犬")
                        '(:contain)))
            '(:contain)))

(defparameter *place*
  (defvocab "place" nil nil
            `#(,(defvocab "森" :place
                        #("木")
                        #("森林公園")
                        '(:contain))
              ,(defvocab "海岸" :place
                        #("水" "砂浜")
                        #("リアス海岸" "千歳海岸")
                        '(:contain))
              ,(defvocab "山" :place
                        #("木" "岩" "森")
                        #("岩手山" "姫神山" "富士山")
                        '(:contain)))
            '(:contain)))

(defparameter *par*
  (defvocab "par" nil nil
            `#(,(defvocab "が" nil nil nil nil)
              ,(defvocab "の" nil nil nil nil)
              ,(defvocab "を" nil nil nil nil)
              ,(defvocab "に" nil nil nil nil)
              ,(defvocab "へ" nil nil nil nil)
              ,(defvocab "と" nil nil nil nil)
              ,(defvocab "は" nil nil nil nil)
              ,(defvocab "や" nil nil nil '("や" :sub)))
            '(:contain)))

(defmacro p-if (p fun &body body)
  "pがnil以外ならpにfunを適用した値を返す"
  `(let ((val ,p))
     (if val (,fun val) ,@body)))

(defun get-contain (vocab)
  "指定したトップレベル要素の含む要素をランダムに返す"
  (p-if (assoc vocab *abstruct-list*) (lambda (x) (choose (symbol-value (cdr x))))
        (choose (vocab-contain vocab))))

(defmacro choose (vec)
  "配列から1つランダムに取り出す"
  `(svref ,vec (random (length ,vec))))

(defun get-sibling (vocab)
  "兄弟要素を返す"
  (choose (vocab-contain (vocab-parent vocab))))

;(defun expand-sym (sym)

(defun eval-rule (rule)
  "ルールの評価"
  (format nil "~{~A~^~}" (loop for x in rule collect (eval-sym x))))

(defun eval-sym (sym)
  "シンボルの展開"
  (if (vocab-p sym)
    (if (vocab-rule sym)
      (if (member :contain (vocab-rule sym))
        (eval-rule (subst (choose (vocab-contain sym)) :contain (vocab-rule sym)))
        (eval-rule (vocab-rule sym)))
      (vocab-sym sym))
    (if (typep sym 'list)
      (eval-rule sym)
      sym)))
