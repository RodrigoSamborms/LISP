(defun print-sign (n)
   (format t "~A~%"
        (if (< n 0) 
            "negativo" 
            "positive o  zero")))

(print-sign -10)