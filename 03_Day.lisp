(defvar *mealCost* (read))
(defvar *tipPercent* (read))
(defvar *taxPercent* (read))

(defun tip (tipPercent cost)
    (* cost (/ tipPercent 100)))

(defun tax (taxPercent cost)
    (* cost (/ taxPercent 100)))

(defvar *tax* (tax *taxPercent* *mealCost*))
(defvar *tip* (tip *tipPercent* *mealCost*))

(defvar *totalCost* (round (+ *mealCost* *tax* *tip* )))

(format t "The total meal cost is ~d dollars." *totalCost*)
