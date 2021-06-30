; vim: noai:ts=2:sw=2:et: 
(defpackage :espy-misc-utils 
  (:use :cl)  (:nicknames :etc))
(in-package :etc)

(defun loading (file)
  (format *error-output* "; loading ~(~a~) ...~%" file)
  (handler-bind ((style-warning #'muffle-warning))
    (load file)))

(loading "config"); aa
;-------------------------------------
(defmacro aif (test yes &optional no)
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro whale (expr &body body) 
  `(do ((a ,expr ,expr)) ((not a)) ,@body))

(defmacro ? (p x &rest xs)
  (if (null xs) `(getf ,p ,x) `(? (getf ,p ,x) ,@xs)))

(defmacro bad (x &rest y)
  `(assert ,x () ,@y))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

;-------------------------------------
(let* ((seed0      10013)
       (seed       seed0)
       (multiplier 16807.0d0)
       (modulus    2147483647.0d0))
  (defun srand (&optional (n seed0))  (setf seed n))
  (defun randi (n) (floor (* n (/ (randf 1000.0) 1000))))
  (defun randf (n) 
    (setf seed (mod (* multiplier seed) modulus))
    (* n (- 1.0d0 (/ seed modulus)))))

; ------------------------------------
(defun halt (&optional (status 0)) (sb-ext:exit :code status))
(defun argv () sb-ext:*posix-argv*)
; ------------------------------------
(defmethod num? ((x number))  x)
(defmethod num? ((x string))
  (let ((y (read-from-string x)))
    (if (typep y 'number) y x)))

; ---------------------------------------------
(defun cli (&key (plist  (deepcopy +config+)) 
                 (help   "")
                 (args   (cdr (deepcopy (argv))))
                 (now    (getf plist :all)))
   (whale (pop args)
      (setf a (read-from-string a))
      (cond ((equalp a :H)  (format t "~a~%" help))
            ((getf plist a) (setf now (getf plist a)))
            ((getf now a)   (setf (getf now a) 
                                  (read-from-string (car args))))
            ((keywordp a)   (format t "; Skipping [~a]" a))))
   plist)

;----------------------------------------------
(defun deepcopy (x)
   (if (consp x) (mapcar #'deepcopy x) x))

(defun powerset (lst)
  (let ((out (list nil)))
    (dolist (x lst out)
      (dolist (tmp out)
        (push (cons x tmp) out)))))

(defun color (s  c &optional (str t)) 
 (let((colors '((black . 30) (red . 31) (green . 32)  
                (yellow . 33)  (blue . 34)  (magenta . 35)
                (cyan . 36) (white .37))))
   (format str "~c[~a;1m~a~c[0m" 
           #\ESC (cdr (assoc c colors)) s #\ESC)))

(defun red (s) (color s 'red nil))
(defun green (s) (color s 'green nil))
(defun yellow (s) (color s 'yellow nil))
; ----------------------------------------------
(defun str->words (s0) 
  (labels ((whitep (c) (member c '(#\space #\tab)))
           (worker (str &optional (lo 0))
                   (aif (position #\comma str :start lo)
                        (cons (subseq str lo  it) 
                              (worker str (1+ it)))
                        (list (subseq str lo)))))
    (let ((s1 (remove-if  #'whitep s0)))
      (unless (zerop (length s1)) (worker  s1)))))

(defun file->words (f fn)
  (with-open-file (s f) 
    (whale (read-line s nil)
           (aif (str->words a)
                (funcall fn (mapcar #'num? it))))))

