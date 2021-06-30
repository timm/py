(load "code")
(code "etc")

(defvar *tests* nil)
(defvar *fails* 0)

(defmacro deftest (name params  doc  &body body)
  `(progn (pushnew  ',name *tests*) 
          (defun ,name ,params ,doc ,@body)))

(defun demos (my &optional what)
  (dolist (one *tests*)
    (let ((doc (documentation one 'function)))
    (when (or (not what) (eql one what))
      (srand (my :rand :seed))
      (multiple-value-bind (_ err)
         (ignore-errors (funcall one (deepcopy my)))
         (incf *fails* (if err 1 0))
         (if err
           (format t "~&~a [~a] ~a ~a~%" 
             (color "✖" 'red   nil) one doc (color err 'yellow nil))
           (format t "~&~a [~a] ~a~%"    
             (color "✔" 'green nil) one doc)))))))

(deftest _aif (_)
  "testing test"
  (aif (- 3 4)
    (assert (= it 1))))

(demos (cli))
(halt *fails*)
