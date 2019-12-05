
(defpackage :aoc2019-intcode
  (:use :cl :aoc2019-utils)
  (:export
   :+op-add+
   :+op-mul+
   :+op-end+
   :read-program
   :make-vm
   :run-vm
   :vm-value))

(in-package :aoc2019-intcode)

(defconstant +op-add+ 1)
(defconstant +op-mul+ 2)
(defconstant +op-end+ 99)

(defun read-program (string)
  (map 'vector #'parse-integer (ppcre:split "," string)))

(defun read-input-program (number)
  (read-program (read-input-file number)))

(define-condition program-end (condition)
  ())

(defclass vm ()
  ((memory
    :type (vector integer)
    :accessor vm-memory
    :initarg :memory)
   (current-address
    :type (integer 0)
    :accessor vm-current-address
    :initarg :current-address
    :initform 0)))

(defun make-vm (program)
  (when (zerop (length program))
    (error "empty program"))
  (let ((memory (make-array (length program) :element-type 'integer
                                             :initial-contents program)))
    (make-instance 'vm :memory memory)))

(defun vm-value (vm address &optional (offset 0))
  (aref (vm-memory vm) (+ address offset)))

(defun (setf vm-value) (value vm address &optional (offset 0))
  (setf (aref (vm-memory vm) (+ address offset)) value))

(defun step-vm (vm)
  (with-slots (current-address memory) vm
    (let ((op (vm-value vm current-address)))
      (flet ((check-nb-args (n)
               (when (>= (+ current-address n) (length memory))
                 (error "truncated op ~S at address ~D" op current-address))))
        (cond
          ((= op +op-add+)
           (check-nb-args 4)
           (let ((a (vm-value vm current-address 1))
                 (b (vm-value vm current-address 2))
                 (destination (vm-value vm current-address 3)))
             (setf (vm-value vm destination)
                   (+ (vm-value vm a) (vm-value vm b))))
           (incf current-address 4))
          ((= op +op-mul+)
           (check-nb-args 4)
           (let ((a (vm-value vm current-address 1))
                 (b (vm-value vm current-address 2))
                 (destination (vm-value vm current-address 3)))
             (setf (vm-value vm destination)
                   (* (vm-value vm a) (vm-value vm b))))
           (incf current-address 4))
          ((= op +op-end+)
           (signal 'program-end))
          (t
           (error "unknown opcode ~S" op))))))
  nil)

(defun run-vm (vm)
  (handler-case
      (loop
        (step-vm vm))
    (program-end ()
      nil)))
