;; Enter your code here. Read input from STDIN. Print output to STDOUT
(defun array6 ()
    (setq *an-array* (make-array '(6 6) :initial-element 0))
    (setq *max-baris* 6)
    (setq *max-kolom* 6)
    (loop for baris upfrom 0 to (- *max-baris* 1) do
          (loop for kolom upfrom 0 to (- *max-baris* 1) do
                (setq angka (read))
                (setf (aref *an-array* baris kolom) angka)))
    *an-array*)

(defun a-graph (suatu-array6 suatu-lebar suatu-baris suatu-kolom)
    (setq countz 0)
    (setq tengah 1)
    (loop for baris upfrom suatu-baris to (+ suatu-baris (- suatu-lebar 1)) do
          (loop for kolom upfrom suatu-kolom to (+ suatu-kolom (- suatu-lebar 1)) do
                (unless (and (equal (+ tengah suatu-baris) baris) (not (equal (+ tengah suatu-kolom) kolom)))
                    (setf countz (+ countz (aref suatu-array6 baris kolom))))))
    countz)

(defun array-a (suatu-array6 suatu-lebar)
    (setq list-array-a (list))
    (setq arr-len (array-dimension *an-array* 0))
    (setq max-arr-index (- arr-len 1))
    (loop for baris upfrom 0 to (- max-arr-index 2) do
              (loop for kolom upfrom 0 to (- max-arr-index 2) do
                    (setf list-array-a (append list-array-a (list (a-graph suatu-array6 suatu-lebar baris kolom))))))
    list-array-a)

(defun maximum (list)   (reduce #'max list))

(setq *suatu-array* (array6))
(setq *list-a-graph* (array-a *suatu-array* 3))
(format t "~a" (maximum *list-a-graph*))
