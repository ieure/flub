;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.expr-test
  (:use [clojure.test]
        [the.parsatron :only [run >>]])
  (:require [flub.parser.expr :as e]))

(deftest test-expr
  (is (= [:expr [:or [:term [:register 10] [[:inc 1] [:shr 3]]] [:term 127]]]
         (run e/expr "REGA INC SHR 3 OR 7F")))
  (is (= [:term 0x7FFF] (run e/expr "7FFF")))
  (is (= [:expr [:and [:term [:register 10] [[:dec 1]]]
                 [:term 127 [[:shr 1] [:inc 1]]]]]
         (run e/expr "REGA DEC AND 7F SHR INC")))
  (is (= [:expr [:and [:term [:register 10]] [:expr [:or [:term 127] [:term :FF]]]]]
         (run e/expr "REGA AND 7F OR FF"))))
