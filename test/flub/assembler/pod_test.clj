;; -*- coding: utf-8 -*-
;;
;; © 2014, 2015 Ian Eure.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.assembler.pod-test
  (:use [clojure.test]
        [clojure.pprint])
  (:require [flub.assembler.core :as asm]))

(deftest ^:fixme test-poddef
  (let [ast [:PODDEF
             [:FORCELN "HALT" [:DEC "0"]]
             [:FORCELN "BR/ACK" [:DEC "1"]]
             [:FORCELN "INTR" [:DEC "3"]]
             [:BUS_TEST_ADDR [:HEX "1000FFE"]]
             [:RUN_UUT_ADDR [:HEX "F6000000"]]]]
    (pprint ast)
    (pprint (asm/ast->bytes ast))))
