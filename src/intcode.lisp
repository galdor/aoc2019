
(defpackage :aoc2019-intcode
  (:use :cl :aoc2019-utils)
  (:export
   :read-program
   :read-input-program
   :make-vm
   :run-vm
   :vm-value
   :vm-add-input
   :vm-output))

(in-package :aoc2019-intcode)

(defconstant +op-add+ 1)
(defconstant +op-mul+ 2)
(defconstant +op-in+ 3)
(defconstant +op-out+ 4)
(defconstant +op-jit+ 5)
(defconstant +op-jif+ 6)
(defconstant +op-lt+ 7)
(defconstant +op-eq+ 8)
(defconstant +op-end+ 99)

(defconstant +parameter-mode-position+ 0)
(defconstant +parameter-mode-immediate+ 1)

(defun read-program (string)
  (map 'vector #'parse-integer (ppcre:split "," string)))

(defun read-input-program (number)
  (read-program (read-input-file number)))

(define-condition program-end (condition)
  ())

(defun op-code (op)
  (mod op 100))

(defun op-parameter-mode (op n)
  (nth-value 0 (truncate (mod op (expt 10 (+ n 2))) (expt 10 (+ n 1)))))

(defclass vm ()
  ((memory
    :type (vector integer)
    :accessor vm-memory
    :initarg :memory)
   (current-address
    :type (integer 0)
    :accessor vm-current-address
    :initarg :current-address
    :initform 0)
   (input
    :type list
    :accessor vm-input
    :initarg :input
    :initform nil)
   (output
    :type list
    :accessor vm-output
    :initarg :output
    :initform nil)))

(defun make-vm (program)
  (when (zerop (length program))
    (error "empty program"))
  (let ((memory (make-array (length program) :element-type 'integer
                                             :initial-contents program)))
    (make-instance 'vm :memory memory)))

(defun vm-add-input (vm value)
  (with-slots (input) vm
    (setf input (append input (list value)))))

(defun vm-add-output (vm value)
  (with-slots (output) vm
    (setf output (append output (list value)))))

(defun vm-read-input (vm)
  (with-slots (input) vm
    (when (null input)
      (error "no input value available"))
    (prog1 (car input)
      (setf input (cdr input)))))

(defun vm-value (vm address &optional (offset 0))
  (aref (vm-memory vm) (+ address offset)))

(defun (setf vm-value) (value vm address &optional (offset 0))
  (setf (aref (vm-memory vm) (+ address offset)) value))

(defun step-vm (vm)
  (with-slots (current-address memory) vm
    (let* ((op (vm-value vm current-address))
           (opcode (op-code op)))
      (labels ((parameter (n)
                 (when (>= (+ current-address n) (length memory))
                   (error "truncated op ~S at address ~D" op current-address))
                 (vm-value vm current-address n))
               (parameter-value (n)
                 (cond
                   ((= (op-parameter-mode op n) +parameter-mode-position+)
                    (vm-value vm (parameter n)))
                   ((= (op-parameter-mode op n) +parameter-mode-immediate+)
                    (parameter n))
                   (t
                    (error "invalid parameter mode for op ~S at address ~D"
                           op current-address)))))
        (cond
          ((= opcode +op-add+)
           (setf (vm-value vm (parameter 3))
                 (+ (parameter-value 1) (parameter-value 2)))
           (incf current-address 4))
          ((= opcode +op-mul+)
           (setf (vm-value vm (parameter 3))
                 (* (parameter-value 1) (parameter-value 2)))
           (incf current-address 4))
          ((= opcode +op-in+)
           (let ((value (vm-read-input vm)))
             (setf (vm-value vm (parameter 1)) value))
           (incf current-address 2))
          ((= opcode +op-out+)
           (let ((value (vm-value vm (parameter 1))))
             (vm-add-output vm value))
           (incf current-address 2))
          ((= opcode +op-jit+)
           (if (not (zerop (parameter-value 1)))
               (setf current-address (parameter-value 2))
               (incf current-address 3)))
          ((= opcode +op-jif+)
           (if (zerop (parameter-value 1))
               (setf current-address (parameter-value 2))
               (incf current-address 3)))
          ((= opcode +op-lt+)
           (setf (vm-value vm (parameter 3))
                 (if (< (parameter-value 1) (parameter-value 2)) 1 0))
           (incf current-address 4))
          ((= opcode +op-eq+)
           (setf (vm-value vm (parameter 3))
                 (if (= (parameter-value 1) (parameter-value 2)) 1 0))
           (incf current-address 4))
          ((= opcode +op-end+)
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
