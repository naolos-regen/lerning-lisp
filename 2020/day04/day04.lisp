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

(defparameter *required-keys-p1* 
  (list *birth-year* 
	*issue-year* 
	*expiration-year* 
	*height*
	*hair-color*
	*eye-color*
	*passport-id*))
	;*country-id*))

(defparameter *required-keys-p2*
  `((,*birth-year*      . (1920 2002))
    (,*issue-year*      . (2010 2020))
    (,*expiration-year* . (2020 2030))
    (,*height*          . ((:cm . (150 193)) (:in . (59 76))))
    (,*hair-color*      . #\#)
    (,*eye-color*       . ("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
    (,*passport-id*     . 9)))


(defun validate-p1-p (passport)
  (every (lambda (k)
	   (assoc k passport :test #'string=))
	 *required-keys-p1*))

(defun validate-p2-p (passport)
  (and (validate-p1-p passport)
       (every (lambda (rule)
		(let* ((key (car rule))
			(requirements (cdr rule))
			(value (cdr (assoc key passport :test #'string=))))
		  (case (intern (string-upcase key) :keyword)
		    ((:byr :iyr :eyr)
		     (let ((year (parse-integer value :junk-allowed t)))
		       (and year (<= (first requirements) year (second requirements)))))
		    (:hgt
		     (let ((num (parse-integer value :junk-allowed t))
			   (unit (subseq value (max 0(- (length value) 2)))))
		       (let ((range (cdr (assoc (intern (string-upcase unit) :keyword) requirements))))
			 (and num range (<= (first range) num (second range))))))

		    (:hcl
		     (and (= (length value) 7)
			  (char= (char value 0) #\#)
			  (every (lambda (c) (digit-char-p c 16)) (subseq value 1))))

		    (:ecl
		     (member value requirements :test #'string=))

		    (:pid
		     (and (= (length value) 9)
			  (every #'digit-char-p value)))

		    (t t))))
	      *required-keys-p2*)))



(defun get-cleaned-passports (raw-data)
    (loop for passport-lines in raw-data 
          collect (loop for line in passport-lines
                        append (loop for entry in (split-by-delimeter line #\Space)
                                     collect (let ((pair (split-by-delimeter entry #\:)))
                                               (cons (car pair) (cadr pair)))))))

(defun main (filename)
  (let* ((raw-lines (get-file filename))
	 (passport-groups (seperate-lines raw-lines))
	 (passports (get-cleaned-passports passport-groups)))
    (let ((p1-count (count-if #'validate-p1-p passports))
	  (p2-count (count-if #'validate-p2-p passports)))
      (format t "Year 2020 Day 04 Part 01 ~A~%" p1-count)
      (format t "Year 2020 Day 04 Part 02 ~A~%" p2-count)
      (list p1-count p2-count))))



(main "2020/day04/input.txt")
