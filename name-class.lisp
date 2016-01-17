(defstruct dict-item
  (name "")
  (contain (make-array 16 :fill-pointer 0 :adjustable t)))

(defmacro is-contain (name item)
  `(find ,name (dict-item-contain ,item) :test #'string=))

(defmacro add-to-item (name item)
  `(vector-push-extend ,name (svref (dict-item-contain ,item) (length (dict-item-contain ,item)))))
