;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.pod-test
  (:use [clojure.test]
        [clojure.pprint])
  (:require [clojure.java.io :as io]
            [flub.io.ws :as ws]
            [flub.parser.pod :as pod]))

(deftest test-pod-parse
  (is (= [:POD
          [:FORCELN [:FORCE_LINE "BUSRQ"] [:DEC "4"]]
          [:FORCELN [:FORCE_LINE "WAIT"] [:DEC "5"]]
          [:BUS_ADDR [:HEX "FFFF"]]
          [:UUT_ADDR [:HEX "0000"]]]
         (pod/source->ast (slurp (io/resource "include/Z80.POD"))))))
