(defun str->cells (str) 
  (labels 
    ((whitep (c) (member  c '(#\space #\tab #\newline)))
     (commap (c) (eql c '#\comma))
     (worker (str &optional (lo 0) (white t) hi)
             (if white
               (if (setf hi (position #\comma str :start lo))
                 (cons (subseq str lo  hi) (worker str hi nil))
                 (list  (subseq str lo)))
               (if (setf hi (position-if-not  #'commap str :start lo))
                 (worker str hi t)))))
    (worker (remove-if #'whitep str))))

(defmacro docells ((cells f &optional out) &body body)
  (let ((line (gensym))
        (s    (gensym)))
    `(let (,line)
       (with-open-file (,s ,f) 
         (loop while (setf ,line (read-line ,s nil)) do
           (setf ,cells (str->cells ,line))
           ,@body))
       ,out)))

(docells (cells "../data/auto93.csv") (print cells))
