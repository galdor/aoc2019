
(defpackage :aoc2019-05
  (:use :cl :aoc2019-utils :aoc2019-intcode)
  (:export :run1 :run2 :check))

(in-package :aoc2019-05)

;;; https://adventofcode.com/2019/day/5

(defun run1 ()
  (let ((vm (make-vm (read-input-program 5))))
    (vm-add-input vm 1)
    (run-vm vm)
    (car (last (vm-output vm)))))

(defun run2 ()
  (let ((vm (make-vm (read-input-program 5))))
    (vm-add-input vm 5)
    (run-vm vm)
    (car (last (vm-output vm)))))

(defun check ()
  (check-problem-results 5 8332629 8805067))
