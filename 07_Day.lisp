;; Enter your code here. Read input from STDIN. Print output to STDOUT

(defvar *input* (read))

(defun odd-part (word)
    
    (setq odd "")
    (loop for x from 0 to (- (length word) 1) do
        (cond
            ((equal (mod x 2) 1)
             (setf odd (concatenate 'string odd (subseq word x (+ x 1)))))))
    odd
    )

(defun even-part (word)
    (setq even "")
    (loop for x from 0 to (- (length word) 1) do
        (cond
            ((equal (mod x 2) 0)
             (setf even (concatenate 'string even (subseq word x (+ x 1)))))))
    even
    )

(loop for x from 1 to *input* do
      (setq test (read-line))
      (format t "~a ~a~%" (even-part test) (odd-part test)))
