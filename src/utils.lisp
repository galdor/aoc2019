
(defpackage :aoc2019-utils
  (:use :cl)
  (:export :problem-results
           :check-problem-results
           :input-file-path
           :read-input-file
           :do-input-lines
           :read-input-lines))

(in-package :aoc2019-utils)

(defun problem-run-function (problem-number run-number)
  (let* ((package-name (format nil "AOC2019-~2,'0d" problem-number))
         (package (find-package package-name))
         (symbol-name (format nil "RUN~d" run-number)))
    (unless package
      (error "package ~s not found" package-name))
    (let ((symbol (find-symbol symbol-name package)))
      (unless (find-symbol symbol-name package)
        (error "symbol ~s not found in package ~s" symbol-name package))
      (symbol-function symbol))))

(defun problem-results (number)
  (values (funcall (problem-run-function number 1))
          (funcall (problem-run-function number 2))))

(defun check-problem-results (number expected-result-1 expected-result-2)
  (multiple-value-bind (result1 result2)
      (problem-results number)
    (unless (eql result1 expected-result-1)
      (error "first result is ~s but should be ~s"
             result1 expected-result-1))
    (unless (eql result2 expected-result-2)
      (error "second result is ~s but should be ~s"
             result2 expected-result-2))))

(defun input-file-path (number)
  (let ((file-path (format nil "input/~2,'0d.txt" number)))
    (asdf:system-relative-pathname "aoc2019" file-path)))

(defun read-input-file (number)
  (with-open-file (file (input-file-path number)
                        :element-type 'character
                        :external-format :utf-8)
    (let ((data (make-array (file-length file) :element-type 'character)))
      (read-sequence data file)
      data)))

(defmacro do-input-lines ((number line &optional result-var) &body body)
  (let ((stream (gensym "STREAM-")))
    `(with-open-file (,stream (input-file-path ,number))
       (handler-case
           (loop
             (let ((,line (read-line ,stream)))
               (progn
                 ,@body)))
         (end-of-file ()
           ,result-var)))))

(defun read-input-lines (number &optional (transform #'identity))
  (let ((lines nil))
    (do-input-lines (number line (nreverse lines))
      (push (funcall transform line) lines))))
