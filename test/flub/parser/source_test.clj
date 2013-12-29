;; -*- coding: utf-8 -*-
;;
;; © 2013 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.source-test
  (:use [clojure.test]
        [clojure.pprint]
        [flub.test-util])
  (:require [flub.parser.source :as p]))

(deftest test-reg-assign
  (is (parsed? (p/p "REG2 = REG3 DEC" :start :REG_ASSIGN)))
  (is (parsed? (p/p "REG3 DEC" :start :EXPR)))
  (is (parsed? (p/p "REG3 DEC" :start :TERM))))

(deftest test-address-block
  (is (parsed? (p/p "RAM SHORT @ REG6-REG7" :start :RAM_TEST)))
  (is (parsed? (p/p "REG6-REG7" :start :ADDRESS_BLOCK))))

(deftest test-rom-test
  (is (parsed? (p/p "ROM TEST @ REG8-REG8 OR 7FF SIG REG6" :start :ROM_TEST))))

(deftest test-program-head
  (is (parsed? (p/p "PROGRAM 91   207 BYTES : Frequency - in Utilities.bin" :start :PROGRAM_HEAD))))

(deftest test-label-statement
  (is (parsed? (p/p "C: DPY-DATA ERROR-SENT $1-REC'D $E" :start :STATEMENT)
               "Statements can follow labels")))
