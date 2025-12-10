(defun read-number ()
    (format t "Escribe un numero: ")
    (finish-output)
    (parse-integer (read-line)))

(defun read-and-sum (n)
    (let ((total 0))
        (do ((i (read-number) (read-number))) ((= i 0) total)
            (setf total (+ total i)))))

(princ (read-and-sum 5))