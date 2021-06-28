(defmacro aif (test yes &optional no)
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro whale (expr &body body) 
  `(do ((a ,expr ,expr)) ((not a)) ,@body))

(defun num?(s0)
  (let ((s1 (ignore-errors 
              (with-input-from-string (in s0) (read in)))))
    (if (typep s1 'number) s1 s0)))

(defun blackspace(str) 
  (remove-if #'(lambda (c) (member c '(#\space #\tab))) str))

(defun str->words1 (str &optional (lo 0))
  (aif (position #\comma str :start lo)
       (cons (subseq str lo  it) (str->words1 str (1+ it) ))
       (list (subseq str lo))))

(defun str->words (str) 
  (let ((str (blackspace str)))
    (unless (zerop (length str)) (str->words1 str))))

(defun file->words (f fn)
  (with-open-file (s f) 
    (whale (read-line s nil)
           (funcall fn (mapcar #'num? (str->words a))))))


(file->words "../data/auto93.csv" #'identity)
