(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun find-pairs-that-sum (numbers target-sum)
  (let ((result '()))
    (loop for i from 0 below (length numbers)
          do (loop for j from (1+ i) below (length numbers)
                   do (let ((first (nth i numbers))
                            (second (nth j numbers)))
                        (when (= (+ first second) target-sum)
                          (push (list first second) result)))))
    (nreverse result)))

(defun find-triplets-that-sum (numbers target-sum)
  (let ((result '()))
    (loop for i from 0 below (length numbers)
	  do (loop for j from (1+ i) below (length numbers)
		   do (loop for l from (1+ j) below (length numbers)
			    do (let ((first (nth i numbers))
				     (second (nth j numbers))
				     (third (nth l numbers)))
				 (when (= (+ first second third) target-sum)
				   (push (list first second third) result))))))
    (nreverse result)))

(defun find-sum-combinations (numbers size target)
  (cond
    ((zerop size) (if (zerop target) '(()) nil))
    ((null numbers) nil)
    (t (append
        (mapcar (lambda (rest) (cons (car numbers) rest))
                (find-sum-combinations (cdr numbers) 
                                       (1- size) 
                                       (- target (car numbers))))
        (find-sum-combinations (cdr numbers) size target)))))


(defun parse-integers (lst)
  (mapcar #'parse-integer lst))

(defun multiply-pairs (pairs)
  (mapcar (lambda (pair)
	    (* (first pair) (second pair)))
	  pairs))

(defun multiply-triplets (triplets)
  (mapcar (lambda (triplets)
	    (* (first triplets) (second triplets) (third triplets)))
	  triplets))

(defun multiply-combinations (combinations)
  (print combinations)
  (mapcar (lambda (combo)
	    (reduce #'* combo))
	    combinations))

(let* ((data (parse-integers (get-file "2020/day01/input.txt")))
       (target 2020))
  (format t "Year 2020 Day 01 Part 01 : ~A~%" (multiply-combinations (find-sum-combinations data 2 target)))
  (format t "Year 2020 Day 01 Part 02 : ~A~%" (multiply-combinations (find-sum-combinations data 3 target))))
