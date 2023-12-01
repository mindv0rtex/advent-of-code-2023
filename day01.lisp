(load "utils.lisp")

(defparameter *digit-mapping*
              '(("zero" . "0") ("one" . "1") ("two" . "2") ("three" . "3") ("four" . "4")
                               ("five" . "5") ("six" . "6") ("seven" . "7") ("eight" . "8") ("nine" . "9")))

(defparameter *reversed-digit-mapping*
              (mapcar (lambda (pair)
                        (cons (reverse-string (car pair)) (cdr pair)))
                  *digit-mapping*))

(defun make-regexp (mapping)
  (format nil "(~{~A~^|~})"
    (loop for (name . digit) in mapping
          collect name
          collect digit)))

(defparameter *part1-regex* "[0-9]") ; Just match any digit
(defparameter *part2-regex* (make-regexp *digit-mapping*))
(defparameter *part2-reversed-regex* (make-regexp *reversed-digit-mapping*))

(defun digit-or-name-to-number (digit-or-name)
  (if (digit-char-p (elt digit-or-name 0))
      digit-or-name
      (cdr (assoc digit-or-name *digit-mapping* :test #'string=))))

(defun extract-calibration-value (line regex reversed-regex)
  (let* ((reversed-line (reverse-string line))
         (first-match (ppcre:scan-to-strings regex line))
         (last-match (ppcre:scan-to-strings reversed-regex reversed-line))
         (first-digit (digit-or-name-to-number first-match))
         (last-digit (digit-or-name-to-number (reverse-string last-match))))
    (parse-integer (format nil "~a~a" first-digit last-digit))))

(defun sum-calibration-values (filename part-2?)
  (let* ((lines (uiop:read-file-lines filename))
         (regex (if part-2? *part2-regex* *part1-regex*))
         (reversed-regex (if part-2? *part2-reversed-regex* *part1-regex*)))
    (reduce #'+ (mapcar (lambda (line)
                          (extract-calibration-value line regex reversed-regex))
                    lines))))
