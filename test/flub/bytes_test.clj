;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.bytes-test
  (:use [clojure.test]
        [the.parsatron :only [run]])
  (:require [flub.io.bytes :as b]))

(deftest test-swap-bytes
  (is (= [0x34 0x12 0x78 0x56] (b/swap-bytes [0x12 0x34 0x56 0x78]))))

(deftest test-merge-bytes
  (is (= 0x12345678 (b/merge-bytes [0x34 0x12 0x78 0x56] :little-endian)))
  (is (= (b/merge-bytes [0x34 0x12 0x78 0x56])
         (b/merge-bytes [0x34 0x12 0x78 0x56] :little-endian)))
  (is (= 0x12345678 (b/merge-bytes [0x12 0x34 0x56 0x78] :big-endian))))

(deftest test-bytes->string
  (is (= "8080" (b/bytes->string [56 48 56 48 0 0 0])))
  (is (= "" (b/bytes->string [0 0 0 0 0 0 0])))
  (is (= "" (b/bytes->string []))))
