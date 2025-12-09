(defun ask-name ()
  (format t "Name: ")
  (finish-output)
  (read-line))

(defun ask-and-return ()
   (let ( (name (ask-name) ) )
    (setf name (concatenate 'string "Hola " name "!"))
    (format t "~A~%" name)
     name ) )
     
(princ (ask-and-return) )