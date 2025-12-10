(defun print-sign (n)
   (format t "~A~%"
        (if (< n 0) 
            "negativo" 
            (if (= n 0) "cero"   "positivo"))))

(print-sign 0)