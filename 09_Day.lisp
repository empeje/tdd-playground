;; Enter your code here. Read input from STDIN. Print output to STDOUT

(setq *input* (read))

;; some part of this code taken from http://cl-cookbook.sourceforge.net/strings.html
(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

;; here is my database
(defvar *db* nil)

;; here is the data structure
(defun phone-book (name number)
  (list :name name :number number))

;; here is to add data
(defun add-record (phone-number) (push phone-number *db*))

;; query from Practical Common Lisp Book
(defun select-by-name (name)
    (remove-if-not
     #'(lambda (phone-book) (equal (getf phone-book :NAME) name))
     *db*))

;; input
(loop for x from 1 to *input* do
      (setq *raw* (read-line))
      (setq *data* (split-by-one-space *raw*))
      (setq name (nth 0 *data*))
      (setq the-number (nth 1 *data*))
      (add-record (phone-book name the-number)))

;; select
(loop
 (setq name (read-line nil nil))
 (cond
     ((equal NIL name)
      (quit))
     (t
      (setq the-name (getf (nth 0 (select-by-name name)) :NAME))
      (setq the-number (getf (nth 0 (select-by-name name)) :NUMBER))
      (cond
          ((equal NIL the-name)
           (format t "Not found~%"))
          (t
           (format t "~a=~a~%"  name the-number))))))
    
