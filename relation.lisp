(defun test (routes src dst rel)
  (let ((path (get-path src routes)))
    (if path
      (loop for p in path
            sum (if (eql (cdr p) dst)
                  rel
                  (test
                    (remove-path-to
                      src
                      (get-path (cdr p) routes))
                    (cdr p)
                    dst
                    (/ rel 2))))
      0)))

(defun get-path (src routes)
  (loop for p in routes
        when (eql src (car p))
        collect p))

(defun remove-path-to (dst routes)
  (loop for p in routes
        when (not (eql dst (cdr p)))
        collect p))

;アイテム同士の関連度を求める
;IN: routes(path list), src, dst
;OUT: 全経路の(1 / (2^n))の総和
;n = srcを起点とした各経路のdstまでの距離
