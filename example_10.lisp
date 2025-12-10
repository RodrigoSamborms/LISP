(defun print-sign (n)
   (format t "~A~%"
        (cond 
            ((< n 0) (format t "rama elegida ") "negativo")
            ((= n 0) "cero")
            (t       "positivo"))))

(print-sign -10)