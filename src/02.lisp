
(defpackage :aoc2019-02
  (:use :cl :aoc2019)
  (:export :run1 :run2 :check))

(in-package :aoc2019-02)

;;; https://adventofcode.com/2019/day/2

(defun read-program (string)
  (map 'vector #'parse-integer (ppcre:split "," string)))

(defun eval-program (program)
  (flet ((value (index)
           (aref program index))
         ((setf value) (value index)
           (setf (aref program index) value)))
    (do ((i 0))
        ((= (value i) 99)
         program)
      (ecase (value i)
        (1
         (let ((op1 (value (+ i 1)))
               (op2 (value (+ i 2)))
               (dst (value (+ i 3))))
           (setf (value dst) (+ (value op1) (value op2))))
         (incf i 4))
        (2
         (let ((op1 (value (+ i 1)))
               (op2 (value (+ i 2)))
               (dst (value (+ i 3))))
           (setf (value dst) (* (value op1) (value op2))))
         (incf i 4))))))

(defun run-gravity-program (program noun verb)
  (setf (aref program 1) noun)
  (setf (aref program 2) verb)
  (eval-program program)
  (aref program 0))

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
