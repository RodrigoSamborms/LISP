(defun next-number (n)
    (if (= (mod n 2) 0)
        (progn 
            (format t "Dividir~%") 
            (/ n 2))
        (+ (* 3 n) 1)))
(princ (next-number 6))