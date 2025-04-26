;; Enter your code here. Read input from STDIN. Print output to STDOUT
(defvar *length* (read-line)) ;;; this one actually unused
(defvar *input* (read-line))

;; some part of this code taken from http://cl-cookbook.sourceforge.net/strings.html
(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))
(defun join-string-list (string-list)
    (format nil "~{~A~^ ~}" string-list))

(format t "~a" (join-string-list
   (nreverse
    (split-by-one-space
     *input*))))
