;; -*- coding: utf-8 -*-
;;
;; © 2014, 2015 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.io.bytes-test
  (:use [clojure.test])
  (:require [flub.io.bytes :as b]))

(deftest string->bytes
  (is (= [232 229 236 236 239 160 247 239 242 236 228 0]
         (b/string->bytes "hello world"))))

(deftest test-bytes->string
  (is (= "hello world"
         (b/bytes->string [232 229 236 236 239 160 247 239 242 236 228 0]))))

(deftest test-string-roundtrip
  (is (= "hello world"
         (->> (b/string->bytes "hello world") (b/bytes->string)))))

(deftest test-merge-bytes
  (is (= 0x12345678 (b/merge-bytes [0x34 0x12 0x78 0x56]))))

(deftest test-split-bytes
  (is (= [0x34 0x12 0x78 0x56] (b/split-bytes 0x12345678))))

(deftest test-bytes-rtt
  (let [v 0x12345678]
    (is (= v (-> (b/split-bytes v)
                 (b/merge-bytes))))))
