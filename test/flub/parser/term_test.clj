;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.term-test
  (:use [clojure.test]
        [the.parsatron :only [run digit char >>]])
  (:require [flub.parser.term :as t]))

(deftest test-term-unary-operator
  (is (= [:inc 3] (run t/term-unary-operator "INC3")))
  (is (= [:inc 7] (run t/term-unary-operator "INC 7")))
  (is (= [:inc 1] (run t/term-unary-operator "INC"))))

(deftest test-term
  (is (= '[:term [:register 10] ([:shr 1] [:inc 1])]
         (run t/term "REGA SHR INC")))
  (is (= '[:term [:register 1] ([:dec 1] [:dec 1])]
         (run t/term "REG1 DEC DEC")))
  (is (= '[:term 4660 ([:cpl 1] [:dec 1])]
         (run t/term "1234 CPL DEC")))
  (is (= [:term 0x3FFF] (run t/term "3FFF")))
  (is (= '[:term :FA ([:shr 3])] (run t/term "FA SHR 3")))
  (is (= '[:term [:register 0x0A]] (run t/term "REGA")))
  ;; Should not consume beyond what it can
  (is (= '[:term [:register 0x0A]] (run t/term "REGA OR"))))
