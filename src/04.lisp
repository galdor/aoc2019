
(defpackage :aoc2019-04
  (:use :cl :aoc2019)
  (:export :run1 :run2 :check))

(in-package :aoc2019-04)

;;; https://adventofcode.com/2019/day/4

(defun password-range ()
  (destructuring-bind (min max)
      (ppcre:split "-" (read-input-file 4))
    (values (parse-integer min)
            (parse-integer max))))

(defun password-digits (password)
  (do ((n password)
       (digits (make-array 0 :element-type '(integer 0 9)
                             :adjustable t :fill-pointer 0)))
      ((zerop n)
       (nreverse digits))
    (multiple-value-bind (quotient rest)
        (truncate n 10)
      (vector-push-extend rest digits)
      (setf n quotient))))

(defun password-digit-groups (password)
  (let ((digits (password-digits password)))
    (do ((i 0 (1+ i))
         (groups nil)
         (group-size 1)
         (last-digit nil))
        ((> i (length digits))
         groups)
      (let ((digit (when (< i (length digits))
                     (aref digits i))))
        (when last-digit
          (cond
            ((eql digit last-digit)
             (incf group-size))
            (t
             (push (cons last-digit group-size) groups)
             (setf group-size 1))))
        (setf last-digit digit)))))

(defun digits-has-group-of-two-or-more-p (groups)
  (not (null (find-if (lambda (group)
                        (>= (cdr group) 2))
                      groups))))

(defun digits-has-group-of-two (groups)
  (not (null (find 2 groups :key #'cdr))))

(defun password-valid-p (password &key needs-group-of-two)
  (let* ((digits (password-digits password))
         (groups (password-digit-groups password)))
    (when (digits-has-group-of-two-or-more-p groups)
      (when (and needs-group-of-two (not (digits-has-group-of-two groups)))
        (return-from password-valid-p nil))
      (do ((i 0 (1+ i))
           (last-digit nil))
          ((>= i (length digits))
           t)
        (let ((digit (aref digits i)))
          (when (and last-digit (> last-digit digit))
            (return nil))
          (setf last-digit digit))))))

(defun find-nb-valid-passwords-1 (min max)
  (do ((password min (1+ password))
       (count 0))
      ((> password max)
       count)
    (when (password-valid-p password)
      (incf count))))

(defun find-nb-valid-passwords-2 (min max)
  (do ((password min (1+ password))
       (count 0))
      ((> password max)
       count)
    (when (password-valid-p password :needs-group-of-two t)
      (incf count))))

(defun run1 ()
  (multiple-value-bind (min max)
      (password-range)
    (find-nb-valid-passwords-1 min max)))

(defun run2 ()
  (multiple-value-bind (min max)
      (password-range)
    (find-nb-valid-passwords-2 min max)))

(defun check ()
  (check-problem-results 4 2150 1462))
