(load "etc")
(defpackage :espy-test-suite 
  (:use :cl)  (:nicknames :eg)
  (:import-from :etc :? :bad :aif :it :a))
(in-package :eg)

;-------------------------------------
(defvar *tests* nil)
(defvar *fails* 0)

(defmacro deftest (name params  doc  &body body)
  `(progn (pushnew  ',name *tests*) 
          (defun ,name ,params ,doc ,@body)))

(defun demos (my &optional what)
  (dolist (one *tests*)
    (let ((doc (documentation one 'function)))
    (when (or (not what) (eql one what))
      (etc::srand (? my :rand :seed))
      (multiple-value-bind (_ err)
         (ignore-errors (funcall one (etc::deepcopy my)))
         (incf *fails* (if err 1 0))
         (if err
           (format t "~&~a [~a] ~a ~a~%" 
             (etc::red "✖") one doc (etc::yellow err))
           (format t "~&~a [~a] ~a~%"    
             (etc::green "✔") one doc)))))))

(deftest _aif (_)
  "testing test"
  (aif (- 3 4)
    (bad (= it 1) "aa~a" 2)))

(demos (etc::cli))
(etc::halt *fails*)
