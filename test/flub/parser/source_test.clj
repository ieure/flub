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
  (is (parsed? (p/p "REG3 DEC" :start :TERM)))
  (is (parsed? (p/p "REG1 = REG0 SHR SHR SHR SHR SHR SHR SHR SHR" :start :REG_ASSIGN)))
  (is (parsed? (p/p "REG0 SHR SHR SHR SHR SHR SHR SHR SHR" :start :TERM)))
  (is (parsed? (p/p "REG0 = REG8 AND FFF000 SHR SHR SHR SHR SHR SHR SHR SHR SHR SHR SHR SHR" :start :REG_ASSIGN))))

(deftest test-address-block
  (is (parsed? (p/p "RAM SHORT @ REG6-REG7" :start :RAM_TEST)))
  (is (parsed? (p/p "REG6-REG7" :start :ADDRESS_BLOCK))))

(deftest test-rom-test
  (is (parsed? (p/p "ROM TEST @ REG8-REG8 OR 7FF SIG REG6" :start :ROM_TEST))))

(deftest test-program-head
  ;; This is invalid according to the docs
  (is (parsed? (p/p "PROGRAM 91   207 BYTES : Frequency - in Utilities.bin" :start :PROGRAM_HEAD))))

(deftest test-label-statement
  (is (parsed? (p/p "C: DPY-DATA ERROR-SENT $1-REC'D $E" :start :STATEMENT)
               "Statements can follow labels")))

(deftest test-write
  (is (parsed? (p/p "WRITE @ REGF INC = REG3 AND FF" :start :WRITE))))

(deftest test-read
  (is (parsed? (p/p "READ @ REGF INC" :start :READ))))

(deftest test-read
  (is (parsed-to [:READ [:ADDR [:EXPR [:TERM [:REGISTER "F"]
                                       [:TERM_UNOP "INC"]]]]]
                 (p/p "READ @ REGF INC" :start :READ))))
