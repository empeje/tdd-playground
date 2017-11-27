;; Enter your code here. Read input from STDIN. Print output to STDOUT

(setq *input* (read))

;; some part of this code taken from http://cl-cookbook.sourceforge.net/strings.html
(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

;; here is my database
(defparameter *my-hash* (make-hash-table :test 'equal))

;; here is to add data
(defun add-record (name phone) (setf (gethash name *my-hash*) phone))

;; input
(loop for x from 1 to *input* do
      (setq *raw-data* (read-line))
      (setq *data* (split-by-one-space *raw-data*))
      (setq name (nth 0 *data*))
      (setq the-number (nth 1 *data*))
      (add-record name the-number))

;; select
(loop
 (setq name (read-line nil nil))
 (cond
     ((equal NIL name)
      (quit))
     (t
      (setq the-number (gethash name *my-hash*))
      (cond
          ((equal NIL the-number)
           (format t "Not found~%"))
          (t
           (format t "~a=~a~%"  name the-number))))))
    
