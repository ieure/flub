;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.duop-test
  (:use [clojure.test]
        [the.parsatron :only [run any-char >>]])
  (:require [flub.parser.duop :as d]))

(deftest test-duop
  (is (= [:or [:term [:register 10] [[:inc 1]]] [:term 127]] (run d/duop "REGA INC OR 7F")))
  (is (nil? (run d/duop "7FFF"))))
