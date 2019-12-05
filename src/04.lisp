
(defpackage :aoc2019-04
  (:use :cl :aoc2019-utils)
  (:export :run1 :run2 :check))

(in-package :aoc2019-04)

;;; https://adventofcode.com/2019/day/4

(defun password-range ()
  (destructuring-bind (min max)
      (ppcre:split "-" (read-input-file 4))
    (values (parse-integer min)
            (parse-integer max))))

(defmacro do-passwords ((password &optional return-form) &body body)
  (let ((min (gensym "MIN-"))
        (max (gensym "MAX-")))
    `(multiple-value-bind (,min ,max) (password-range)
       (do ((,password ,min (1+ ,password)))
           ((> ,password ,max)
            ,return-form)
         ,@body))))

(defmacro do-password-digits ((password index digit previous-digit
                               &optional return-form)
                              &rest body)
  (let ((n (gensym "N-"))
        (quotient (gensym "QUOTIENT-"))
        (remainder (gensym "REMAINDER-")))
    `(do ((,n ,password)
          (,index 0 (1+ ,index))
          (,previous-digit nil))
         ((zerop ,n)
          ,return-form)
       (multiple-value-bind (,quotient ,remainder)
           (truncate ,n 10)
         (let ((,digit ,remainder))
           ,@body
           (setf ,previous-digit ,digit))
         (setf ,n ,quotient)))))

(defun password-has-increasing-digits-p (password)
  (do-password-digits (password i digit previous-digit t)
    (when (and previous-digit (> digit previous-digit))
      (return-from password-has-increasing-digits-p nil))))

(defun password-has-group-of-two-or-more-digits-p (password)
  (do-password-digits (password i digit previous-digit)
    (when (and previous-digit (= digit previous-digit))
      (return-from password-has-group-of-two-or-more-digits-p t))))

(defun password-has-group-of-two-digits-p (password)
  (let ((group-size 1))
    (do-password-digits (password i digit previous-digit)
      (when previous-digit
        (cond
          ((eql digit previous-digit)
           (incf group-size))
          (t
           (when (= group-size 2)
             (return-from password-has-group-of-two-digits-p t))
           (setf group-size 1)))))
    (= group-size 2)))

(defun run1 ()
  (let ((count 0))
    (do-passwords (password count)
      (when (and (password-has-increasing-digits-p password)
                 (password-has-group-of-two-or-more-digits-p password))
        (incf count)))))

(defun run2 ()
  (let ((count 0))
    (do-passwords (password count)
      (when (and (password-has-increasing-digits-p password)
                 (password-has-group-of-two-digits-p password))
        (incf count)))))

(defun check ()
  (check-problem-results 4 2150 1462))
