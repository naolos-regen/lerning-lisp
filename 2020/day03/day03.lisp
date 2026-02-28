(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defparameter *square* 0)
(defparameter *tree* 1)
(defparameter *pos-indicator* 2)
(defparameter *p1-traverse* (list 3 1))
(defparameter *p2-traverse* (list (list 1 1) 
				  (list 3 1)
				  (list 5 1)
				  (list 7 1)
				  (list 1 2)))

(defun transform (raw-map)
  (loop for line in raw-map
	collect (map 'list (lambda (c) (if (char= c #\#) *tree* *square*)) line)))

(defun print-map-with-globals (num-map)
  (dolist (row num-map)
    (dolist (cell row)
      (format t "~a" cell))
    (terpri)))

(defun is-tree (num-map map-pos)
  (destructuring-bind (x y) map-pos 
    (let ((cell (nth x (nth y num-map))))
      (if (= cell *tree*) 1 0))))

(defun traverse (map-pos point width)
  (setf (first map-pos) (mod (+ (first map-pos) (first point)) width))
  (incf (second map-pos) (second point))
  map-pos)

(defun solve-day-03 (file-path)
  (let* ((raw-map (get-file file-path))
         (num-map (transform raw-map))
         (width   (length (first num-map)))
         (height  (length num-map)))
    (let ((results (mapcar (lambda (slope)
                             (let ((map-pos (list 0 0))
                                   (tree-count 0))
                               (loop while (< (second map-pos) height) do
                                    (incf tree-count (is-tree num-map map-pos))
                                    (traverse map-pos slope width))
                               tree-count)) 
                           *p2-traverse*)))
      (format t "Year 2020 Day 03 Part 01: ~a~%" (second results))
      (format t "Year 2020 Day 03 Part 02: ~a~%" (apply #'* results))
      results)))

(solve-day-03 "2020/day03/input.txt")
