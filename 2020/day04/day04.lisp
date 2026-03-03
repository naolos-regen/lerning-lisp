(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defgeneric split-by-delimeter (input &optional delimeter))

(defmethod split-by-delimeter ((input string) &optional (delimeter #\Space))
  (loop for i = 0 then (1+ j)
        as j = (position delimeter input :start i)
        collect (subseq input i j)
        while j))

(defmethod split-by-delimeter ((input list) &optional (delimeter #\Space))
  (mapcar (lambda (item)
	    (split-by-delimeter item delimeter))
	  input))
  

(defun seperate-lines (raw-file)
  (loop with current-sublist = nil
	for line in raw-file
	if (string= line "")
	collect (nreverse current-sublist) into result
	and do (setf current-sublist nil)
	else
	do (push line current-sublist)
	finally (return (let ((last-sublist (nreverse current-sublist)))
			  (if last-sublist
			      (append result (list last-sublist))
			      result)))))

(defparameter *birth-year* "byr")
(defparameter *issue-year* "iyr")
(defparameter *expiration-year* "eyr")
(defparameter *height* "hgt")
(defparameter *hair-color* "hcl")
(defparameter *eye-color* "ecl")
(defparameter *passport-id* "pid")
(defparameter *country-id* "cid")
(defparameter *delimeter* #\Space)

(defparameter *required-keys* 
  (list *birth-year* 
	*issue-year* 
	*expiration-year* 
	*height*
	*hair-color*
	*eye-color*
	*passport-id*))
	;*country-id*))

(defun validate (passport)
  (every (lambda (k)
	   (assoc k passport :test #'string=))
	 *required-keys*))

(defun get-cleaned-passports (raw-data)
    (loop for passport-lines in raw-data 
          collect (loop for line in passport-lines
                        append (loop for entry in (split-by-delimeter line #\Space)
                                     collect (let ((pair (split-by-delimeter entry #\:)))
                                               (cons (car pair) (cadr pair)))))))

(defun main (filename)
  (let* ((raw-data (seperate-lines (get-file filename)))
	 (cleaned-passports (get-cleaned-passports raw-data)))
    (count-if (lambda (p) (validate p))
	      cleaned-passports)))

(main "2020/day04/input.txt")
