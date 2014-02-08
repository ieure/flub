;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
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
