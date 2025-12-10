(defun read-number ()
    (format t "Escribe un numero: ")
    (finish-output)
    (parse-integer (read-line)))

(defun read-and-sum (n)
    (let ((total 0))
        (dotimes (i n total)
            (setf total (+ total (read-number))))))

(princ (read-and-sum 5))