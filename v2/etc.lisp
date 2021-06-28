(defmacro aif (test yes &optional no)
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro whale (expr &body body) 
  `(do ((a ,expr ,expr)) ((not a)) ,@body))

(defun str->words1 (str &optional (lo 0))
  (aif (position #\comma str :start lo)
    (cons (subseq str lo  it) (str->words1 str (1+ it) ))
    (list (subseq str lo))))

(defun str->words (str) 
  (let ((str (remove-if #'(lambda (c) (member c '(#\space #\tab))) str)))
    (if (not (zerop (length str))) 
      (str->words1 str)))) 

(defun file->words (f fn)
  (with-open-file (s f) 
    (whale (read-line s nil)
           (aif (str->words a) (funcall fn it)))))

(print (str->words ""))
(print (str->words " "))

(file->words "../data/auto93.csv" #'print)
