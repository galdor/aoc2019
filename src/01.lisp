
(defpackage :aoc2019-01
  (:use :cl :aoc2019)
  (:export :run1 :run2))

(in-package :aoc2019-01)

(defun base-fuel-need (weight)
  (- (floor (/ weight 3)) 2))

(defun total-fuel-need (weight)
  (do* ((fuel (base-fuel-need weight))
        (total-fuel fuel))
       ((<= fuel 0)
        total-fuel)
    (let ((additional-fuel (base-fuel-need fuel)))
      (when (> additional-fuel 0)
        (incf total-fuel additional-fuel))
      (setf fuel additional-fuel))))

(defun run1 ()
  (let ((fuel 0))
    (do-input-lines (1 line fuel)
      (let ((weight (parse-integer line)))
        (incf fuel (base-fuel-need weight))))))

(defun run2 ()
  (let ((fuel 0))
    (do-input-lines (1 line fuel)
      (let ((weight (parse-integer line)))
        (incf fuel (total-fuel-need weight))))))
