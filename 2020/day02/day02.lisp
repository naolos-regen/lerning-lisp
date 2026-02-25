(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defgeneric split-by-delimeter (input &optional delimeter))

(defmethod split-by-delimeter ((input string) &optional (delimeter #\:))
  (loop for i = 0 then (1+ j)
        as j = (position delimeter input :start i)
        collect (subseq input i j)
        while j))

(defmethod split-by-delimeter ((input list) &optional (delimeter #\:))
  (loop for s in input
        collect (split-by-delimeter s delimeter)))

(defgeneric trim (input &optional char-bag))

(defmethod trim ((input string) &optional (char-bag '(#\Space #\Tab #\NewLine #\Return)))
  (string-trim char-bag input))

(defmethod trim ((input list) &optional (char-bag '(#\Space #\Tab #\NewLine #\Return)))
  (loop for s in input
	collect (trim s char-bag)))
	
(defun parse-range-string (str)
  (mapcar #'parse-integer (split-by-delimeter str #\-)))

(defun find-min-instances-within-string (range-list char-str raw-password)
  (destructuring-bind (min-val max-val) range-list
    (let* ((target-char (char char-str 0))
           (actual-count (count target-char raw-password)))
      (<= min-val actual-count max-val))))
	
(defun find-indexing-validation (index-list char-str raw-password)
  (let* ((target-char (char char-str 0))
         (idx1 (1- (first index-list)))
         (idx2 (1- (second index-list)))
         (len (length raw-password))
         (m1 (and (< idx1 len) (char= (char raw-password idx1) target-char)))
         (m2 (and (< idx2 len) (char= (char raw-password idx2) target-char))))
    (not (eq m1 m2))))

(defun process-string-data (data-list)
  (loop for (raw-policy raw-password) in data-list
	for policy-parts = (split-by-delimeter raw-policy #\Space)
	for range-str    = (first policy-parts)
	for char-str     = (second policy-parts)
	for range-list   = (mapcar #'parse-integer (split-by-delimeter range-str #\-))
	for trimmed-pw   = (trim raw-password)
	for is-valid-p1  = (find-min-instances-within-string range-list char-str trimmed-pw)
	for is-valid-p2  = (find-indexing-validation range-list char-str trimmed-pw)
	count is-valid-p1 into is-valid-part1
	count is-valid-p2 into is-valid-part2
	finally (return (list is-valid-part2 is-valid-part1))))


(defun solve-day-02 (file-path)
  (let* ((raw-lines (get-file file-path))
         (data-list (split-by-delimeter raw-lines #\:))
         (results   (process-string-data data-list)))
    (destructuring-bind (p2-count p1-count) results
      (format t "Year 2020 Day 02 Part 01 : ~A~%" p1-count)
      (format t "Year 2020 Day 02 Part 02 : ~A~%" p2-count))))

(solve-day-02 "2020/day02/input.txt")
