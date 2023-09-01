(defun even-length? (lst)
  "Return not-nil if the length of LST is even."
  (= 0 (% (length lst) 2)))

(defun determine-arg-offset (offset)
  "Return 1 if OFFSET is nil, otherwise return its position."
  (if offset
      offset
    1))

(defun check-args (lst &optional offset)
  "Check that each argument in LST is of the correct type, or throws an error.
Offset by OFFSET if set."
  (when lst
    (when (not (even-length? lst))
      (error "Received uneven argument list"))
    (let* ((type-function (car lst))
	   (argument (symbol-value (nth 1 lst))))
      (when (not (funcall type-function argument))
	(error (concat "Expected "
		       (symbol-name type-function)
		       " for argument "
		       (number-to-string (determine-arg-offset offset)))))
      (check-args (cdr (cdr lst))
		  (+ 1 (determine-arg-offset offset))))))

(defun every-other (lst)
  "Only grab even elements of LST."
  (when lst
    (cons
     (car (cdr lst))
     (every-other (cdr (cdr lst))))))

(defmacro define-typed-function (name args &rest body)
  `(defun ,name ,(eval '(every-other args))
     (check-args ',args)
     ,@body))

(provide 'types)
