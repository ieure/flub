;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.literals-test
  (:use [clojure.test]
        [the.parsatron :only [run digit char >>]])
  (:require [flub.parser.literals :as l]))

;; FIXME
(deftest test-chars->long)

(deftest test-constants
  (is (= \A (run l/hex-char "A")))
  (is (= 255 (run l/hex-constant "FF")))
  (is (= 255 (run l/decimal-constant "255")))
  (is (= 42 (run l/binary-constant "101010"))))

(deftest test-register
  (is (= [:register 15] (run l/register "REGF")))
  (is (thrown? RuntimeException (run l/register "REGZ"))))

(deftest test-dash
  (is (nil? (run l/dash "-")))
  (is (nil? (run l/dash " -")))
  (is (nil? (run l/dash "- ")))
  (is (nil? (run l/dash " - "))))

(deftest test-yes-no
  (is (= true (run l/yes-no "YES")))
  (is (= false (run l/yes-no "NO"))))

(deftest test-address-range
  (is (= [0 65535] (run l/address-range "0000-FFFF")))
  (is (= [0 65535] (run l/address-range "0000 - FFFF"))))

(deftest test-sym
  (is (= :foo (run l/sym "foo")))
  (is (= :foo_bar (run l/sym "foo_bar")))
  (is (thrown? RuntimeException (run l/sym "10bar"))))

(deftest test-fluke-string
  (is (= "foo bar" (run l/fluke-string "\"foo bar\"")))
  (is (thrown? RuntimeException (run l/fluke-string "\"foo bar")))
  (is (thrown? RuntimeException (run l/fluke-string "'foo bar\"")))
  (is (thrown? RuntimeException (run l/fluke-string "'foo bar'"))))
