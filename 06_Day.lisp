;; Enter your code here. Read input from STDIN. Print output to STDOUT
(defvar *input* (read))

(loop for x from 1 to 10 do
      (format t "~D x ~D = ~D~%" *input* x (* *input* x)))
