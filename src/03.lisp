
(defpackage :aoc2019-03
  (:use :cl :aoc2019)
  (:export :run1 :run2 :check))

(in-package :aoc2019-03)

;;; https://adventofcode.com/2019/day/3

(defstruct point
  (x 0 :type integer)
  (y 0 :type integer))

(defun point (x y)
  (make-point :x x :y y))

(defun point-zero-p (point)
  (and (zerop (point-x point))
       (zerop (point-y point))))

(defun distance (point1 point2)
  (+ (abs (- (point-x point1) (point-x point2)))
     (abs (- (point-y point1) (point-y point2)))))

(defstruct segment
  start
  end)

(defun segment (start end)
  (make-segment :start start :end end))

(defun segment-length (segment)
  ;; Works since wire segments are either horizontal or vertical
  (with-slots (start end) segment
    (distance start end)))

(defun segment-vertical-p (segment)
  (= (point-x (segment-start segment))
     (point-x (segment-end segment))))

(defun segment-horizontal-p (segment)
  (= (point-y (segment-start segment))
     (point-y (segment-end segment))))

(defun between (n n1 n2)
  (or (<= n1 n n2)
      (<= n2 n n1)))

(defun segments-intersection (segment1 segment2)
  (with-slots ((start1 start) (end1 end)) segment1
    (with-slots ((start2 start) (end2 end)) segment2
      (cond
        ((and (segment-vertical-p segment1)
              (segment-horizontal-p segment2))
         (when (and (between (point-x start1) (point-x start2) (point-x end2))
                    (between (point-y start2) (point-y start1) (point-y end1)))
           (point (point-x start1) (point-y start2))))
        ((and (segment-vertical-p segment2)
              (segment-horizontal-p segment1))
         (when (and (between (point-x start2) (point-x start1) (point-x end1))
                    (between (point-y start1) (point-y start2) (point-y end2)))
           (point (point-x start2) (point-y start1))))
        (t
         nil)))))

(defun read-wire (string)
  (let ((segments nil)
        (p (point 0 0)))
    (dolist (string (ppcre:split "," string) (nreverse segments))
      (let ((direction (char string 0))
            (length (parse-integer string :start 1)))
        (let ((p2 (case direction
                    (#\U (point (point-x p) (+ (point-y p) length)))
                    (#\D (point (point-x p) (- (point-y p) length)))
                    (#\R (point (+ (point-x p) length) (point-y p)))
                    (#\L (point (- (point-x p) length) (point-y p))))))
          (push (segment p p2) segments)
          (setf p p2))))))

(defun read-input-wires ()
  (mapcar 'read-wire (read-input-lines 3)))

(defun wire-intersections (wire1 wire2)
  (let ((intersections nil)
        (length1 0)
        (length2 0))
    (dolist (segment1 wire1)
      (setf length2 0)
      (dolist (segment2 wire2)
        (let ((intersection (segments-intersection segment1 segment2)))
          (when (and intersection (not (point-zero-p intersection)))
            (let ((length (+ length1
                             (distance (segment-start segment1) intersection)
                             length2
                             (distance (segment-start segment2) intersection))))
              (push (cons intersection length) intersections)))
          (incf length2 (segment-length segment2))))
      (incf length1 (segment-length segment1)))
    intersections))

(defun closest-intersection-distance (wire1 wire2)
  (let* ((intersections (wire-intersections wire1 wire2))
         (distances (mapcar (lambda (intersection)
                              (distance (point 0 0) (car intersection)))
                            intersections)))
    (reduce #'min distances)))

(defun closest-intersection-wire-length (wire1 wire2)
  (let* ((intersections (wire-intersections wire1 wire2))
         (lengths (mapcar #'cdr intersections)))
    (reduce #'min lengths)))

(defun run1 ()
  (destructuring-bind (wire1 wire2)
      (read-input-wires)
    (closest-intersection-distance wire1 wire2)))

(defun run2 ()
  (destructuring-bind (wire1 wire2)
      (read-input-wires)
    (closest-intersection-wire-length wire1 wire2)))

(defun check ()
  (check-problem-results 3 1337 65356))
