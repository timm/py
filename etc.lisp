(defun s->lines (s &optional (line (read-line  s nil)))
  (if line
    (cons (s->words line)  (s->lines s))))

(defun s->words
