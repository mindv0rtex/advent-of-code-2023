(defun reverse-string (string)
  (coerce (reverse (coerce string 'list)) 'string))