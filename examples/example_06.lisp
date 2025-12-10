(defun print-sign (n)
   (if (< n 0) 
        (format t "negativo~%") 
        (format t "positive o  zero~% ")))

(print-sign 10)