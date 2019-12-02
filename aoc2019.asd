
(defsystem "aoc2019"
    :description "Advent of Code 2019"
    :author "Nicolas Martyanoff <khaelin@gmail.com>"
    :licence "ISC"
    :depends-on ("cl-ppcre")
    :pathname "src"
    :serial t
    :components ((:file "aoc")
                 (:file "01")
                 (:file "02")))
