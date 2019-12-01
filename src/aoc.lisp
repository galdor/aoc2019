
(defpackage :aoc2019
  (:use :cl)
  (:export :input-file-path
           :do-input-lines
           :read-input-lines))

(in-package :aoc2019)

(defun input-file-path (number)
  (let ((file-path (format nil "input/~2,'0d.txt" number)))
    (asdf:system-relative-pathname "aoc2019" file-path)))

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

(defun read-input-lines (path &optional (transform #'identity))
  (let ((lines nil))
    (do-input-lines (path line (nreverse lines))
      (push (funcall transform line) lines))))
