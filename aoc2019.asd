
(defsystem "aoc2019"
    :description "Advent of Code 2019"
    :author "Nicolas Martyanoff <khaelin@gmail.com>"
    :licence "ISC"
    :depends-on ("cl-ppcre")
    :pathname "src"
    :serial t
    :components ((:file "utils")
                 (:file "intcode")
                 (:file "01")
                 (:file "02")
                 (:file "03")
                 (:file "04")
                 (:file "05")))
