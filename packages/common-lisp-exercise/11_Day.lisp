;; Enter your code here. Read input from STDIN. Print output to STDOUT

(defun count-conseq-bin (number)
    (setq countz 0)
    (loop while (> number 0) do
          (setf number (logand number (ash number 1)))
          (setf countz (1+ countz)))
    countz)

(setq *input* (read))
(format t "~s" (count-conseq-bin *input*))
