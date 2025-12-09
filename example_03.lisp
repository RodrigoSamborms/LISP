(defun ask-name ()
  (format t "Name: ")
  (finish-output)
  (read-line))
  
(defun ask-and-return ()
   (let ( (name (ask-name) ) 
    (another (ask-name)))
    (format t "Hola ~A and ~A~%" name another)
     name ) )
     
(princ (ask-and-return) )