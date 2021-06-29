; vim: noai:ts=2:sw=2:et: 
(let (gotten)
  (defun code (&rest files)
    (mapc #'(lambda(file)
              (unless (member file gotten :test 'equalp)
                (push file gotten)
                (format *error-output* "; ~(~a~).lisp~%" file)
                (handler-bind ((style-warning #'muffle-warning))
                  (load file))))
          files)))

