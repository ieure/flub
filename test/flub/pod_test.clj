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

(deftest all-pod-files-parse
  (testing "1802.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/1802.POD"))))))

  (testing "6502.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/6502.POD"))))))

  (testing "6800.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/6800.POD"))))))

  (testing "68000.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/68000.POD"))))))

  (testing "6802.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/6802.POD"))))))

  (testing "6809.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/6809.POD"))))))

  (testing "6809E.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/6809E.POD"))))))

  (testing "8041.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/8041.POD"))))))

  (testing "8048.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/8048.POD"))))))

  (testing "8080.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/8080.POD"))))))

  (testing "8085.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/8085.POD"))))))

  (testing "8086.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/8086.POD"))))))

  (testing "8086MX.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/8086MX.POD"))))))

  (testing "8088.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/8088.POD"))))))

  (testing "8088MX.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/8088MX.POD"))))))

  (testing "9900.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/9900.POD"))))))

  (testing "Z80.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/Z80.POD"))))))

  (testing "Z8000.POD"
    (is (= :POD (first (pod/file->ast (io/resource "include/Z8000.POD")))))))
