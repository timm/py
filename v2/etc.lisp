(load "code")
(code "config")

(defmacro aif (test yes &optional no)
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro whale (expr &body body) 
  `(do ((a ,expr ,expr)) ((not a)) ,@body))

(defmacro ?? (p x &rest xs)
  (if (null xs) `(getf ,p ,x) `(?? (getf ,p ,x) ,@xs)))

(defmacro my (x &rest y) `(?? +config+ ,x ,@y))

; ------------------------------------
(defun stop () (sb-ext:exit))

(defun thing!(s)
  (ignore-errors 
    (with-input-from-string (in s) (read in))))

(defmethod num? ((x number)) x)
(defmethod num? ((x string))
  (let ((y (read-from-string x)))
    (if (typep y 'number) y x)))

; ---------------------------------------------
(defun cli (&key (options (copy-list +config+)) 
                 (now :all)
                 (help ""))
  (let ((argv (cdr (copy-list sb-ext:*posix-argv*)))
        (now (getf options now)))
    (whale (pop argv)
           (setf a (read-from-string a))
           (cond  ((equalp a :H)
                   (format t "~a~%" help))
                  ((getf options a)
                   (setf now (getf options a)))
                  ((getf now a)
                   (setf (getf now a) 
                           (read-from-string (car argv))))
                  ((keywordp a) 
                   (format t "; WARNING: ignoring [~a]" a))))
    options))

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
                (funcall fn (mapcar #'num?g it))))))

(print (cli))
