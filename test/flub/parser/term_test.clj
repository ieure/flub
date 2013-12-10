;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.term-test
  (:use [clojure.test]
        [the.parsatron :only [run any-char >>]])
  (:require [flub.parser.term :as t]))

(deftest test-unop
  (is (= [:inc 3] (run t/unop "INC3")))
  (is (= [:inc 7] (run t/unop "INC 7")))
  (is (= [:inc 1] (run t/unop "INC"))))

(deftest test-unops
  (is (= [[:shr 1] [:shr 2] [:inc 3]] (run (t/unops) " SHR SHR 2 INC 3"))))

(deftest test-has-unop?
  (is (true? (run t/has-unop? "INC")))
  (is (false? (run t/has-unop? "3FFF")))
  (is (false? (run t/has-unop? "name")))
  (is (true? (run t/has-unop? "SHR")))
  (is (true? (run t/has-unop? "INC SHR DEC"))))

(deftest test-bare-term
  (is (= [:term [:register 10]] (run t/bare-term "REGA")))
  (is (= [:term 0x1234] (run t/bare-term "1234")))
  (is (= [:term 0x3FFF] (run t/bare-term "3FFF")))
  (is (= [:term :FA] (run t/bare-term "FA")))
  (is (= [:term [:register 0x0A]] (run t/bare-term "REGA OR")))
  (is (= \space (run (>> t/bare-term (any-char)) "REGA OR"))))

(deftest test-unop-term
  (is (= [:term [:register 10] '([:inc 1])]
         (run t/unop-term "REGA INC")))
  (is (= [:term [:register 11] '([:inc 2])]
         (run t/unop-term "REGB INC2")))
  (is (= [:term [:register 12] '([:inc 3])]
         (run t/unop-term "REGC INC 3")))
  (is (= '[:term [:register 10] ([:inc 1] [:shr 1] [:dec 1])]
         (run t/unop-term "REGA INC SHR DEC")))
  (is (= '[:term [:register 10] ([:inc 1] [:shr 1] [:dec 1])]
         (run t/unop-term "REGA INC SHR DEC   ")))
  (is (= '[:term [:register 10] ([:inc 1] [:shr 1] [:dec 1])]
         (run t/unop-term "REGA INC SHR DEC OR  "))))

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
