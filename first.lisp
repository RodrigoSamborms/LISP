(defun print-sign (n)
    (format t "~A~%"
        (if (< n 0)
            "negative"
            (if (= n 0) 
                "zero"
                "positive" 
                ))))
(print-sign 0)
