(defun sum-list (l)
        (let ((total 0))
                (dolist (v l total)
                        (setf total (+ total v)))))

(sum-list '(1 2 3 4))
