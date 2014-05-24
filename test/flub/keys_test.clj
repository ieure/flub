;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.keys-test
  (:use [clojure.test])
  (:require [flub.keys :as k]))

(deftest test-normalize
  (is (= :reg (k/normalize :reg)))
  (is (= :reg (k/normalize "REG")))
  (is (= :0 (k/normalize 0)))
  (is (= :a (k/normalize \a)))
  (is (= :incr (k/normalize 'incr)))
  (is (thrown? IllegalArgumentException (k/normalize (fn [])))))

(deftest test-roundtrip
  (doseq [[s v] k/key-table]
    (is (= v (k/key s)))
    (is (= s (k/key-for v)))))

(deftest test-key
  (is (thrown? IllegalArgumentException (k/key :foo))))

(deftest test-keys
  (is (= [0x27 0x28] (k/keys :loop :stop))))

(deftest test-keys-for
  (is (= [:loop :stop] (k/keys-for [0x27 0x28]))))
