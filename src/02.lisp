
(defpackage :aoc2019-02
  (:use :cl :aoc2019-utils :aoc2019-intcode)
  (:export :run1 :run2 :check))

(in-package :aoc2019-02)

;;; https://adventofcode.com/2019/day/2

(defun run-gravity-program (program noun verb)
  (let ((vm (make-vm program)))
    (setf (vm-value vm 1) noun)
    (setf (vm-value vm 2) verb)
    (run-vm vm)
    (vm-value vm 0)))

(defun run1 ()
  (let ((program (read-program (read-input-file 2))))
    (run-gravity-program program 12 2)))

(defun run2 ()
  (let ((program (read-program (read-input-file 2)))
        (target 19690720))
    (loop for noun from 0 to 99 do
      (loop for verb from 0 to 99 do
        (when (= (run-gravity-program (copy-seq program) noun verb) target)
          (let ((result (parse-integer (format nil "~D~D" noun verb))))
            (return-from run2 result)))))))

(defun check ()
  (check-problem-results 2 3101844 8478))
