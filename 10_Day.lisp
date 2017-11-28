;; Enter your code here. Read input from STDIN. Print output to STDOUT

(defun factorial (input)
    (cond 
        ((equal 1 input)
         1)
        (t
         (* input (factorial (- input 1))))))

(setq *input* (read))
(format t "~a" (factorial *input*))
