;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.io.ws-test
  (:use [clojure.test])
  (:require [flub.io.ws :as ws]))

(deftest test-normalize-newlines
  (testing "Unix newlines are untouched"
    (is (= "foo\nbar\n" (ws/normalize-newlines "foo\nbar\n"))))

  (testing "DOS newlines -> UNIX"
    (is (= "foo\nbar\n" (ws/normalize-newlines "foo\r\nbar\r\n"))))

  (testing "Mac newlines -> UNIX"
    (is (= "foo\nbar\n" (ws/normalize-newlines "foo\rbar\r"))))

  (testing "Mixed newlines -> UNIX"
    (is (= "foo\nbar\nbaz\n" (ws/normalize-newlines "foo\nbar\rbaz\r\n")))))

(deftest test-truncate-eof
  (is (= "foo" (ws/truncate-eof "foo"))))
