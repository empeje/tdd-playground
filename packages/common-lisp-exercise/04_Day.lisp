;; Enter your code here. Read input from STDIN. Print output to STDOUT

(defvar *input* (read))

(defun select-options (input)
    (cond
        ((equal (mod *input* 2) 1)
         (format t "Weird"))
        ((and (> *input* 1) (< *input* 6))
         (format t "Not Weird"))
        ((and (> *input* 5) (< *input* 21))
         (format t "Weird"))
        (t
         (format t "Not Weird"))))

(select-options *input*)
