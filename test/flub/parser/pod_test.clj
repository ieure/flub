;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.pod-test
  (:use [clojure.test]
        [clojure.pprint]
        [flub.test-util])
  (:require [clojure.java.io :as io]
            [flub.io.ws :as ws]
            [flub.parser.pod :as pod]))

(deftest test-pod-parse
  (is (parsed-to
       [:PODDEF
        [:FORCELN "BUSRQ" [:DEC "4"]]
        [:FORCELN "WAIT" [:DEC "5"]]
        [:BUS_TEST_ADDR [:HEX "FFFF"]]
        [:RUN_UUT_ADDR [:HEX "0000"]]]
       (pod/source->ast (slurp (io/resource "include/Z80.POD"))))))

(defmacro test-pod [^String pod]
  `(testing (str ~pod " pod")
     (let [ast# (pod/file->ast (io/resource (format "include/%s.POD" ~pod)))]
       (is (~'parsed? ast#))
       (is (= :PODDEF (first ast#))))))

(deftest all-pod-files-parse
  (test-pod "1802")
  (test-pod "6502")
  (test-pod "6800")
  (test-pod "68000")
  (test-pod "6802")
  (test-pod "6809")
  (test-pod "6809E")
  (test-pod "8041")
  (test-pod "8048")
  (test-pod "8080")
  (test-pod "8085")
  (test-pod "8086")
  (test-pod "8086MX")
  (test-pod "8088")
  (test-pod "8088MX")
  (test-pod "9900")
  (test-pod "Z80")
  (test-pod "Z8000"))
