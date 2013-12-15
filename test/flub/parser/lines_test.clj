;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.lines-test
  (:use [clojure.test]
        [the.parsatron :only [run digit char >>]])
  (:require [flub.parser.lines :as l]))

(deftest test-lines
  (is (nil? (run l/ws "  \t   \t ")))
  (is (thrown? RuntimeException (run l/ws "")))
  (is (nil? (run l/optws "")))
  (is (nil? (run l/optws "  \t   \t ")))
  (is (nil? (run l/reqws "  \t   \t ")))
  (is (thrown? RuntimeException (run l/reqws "")))

  ;; EOL
  (is (nil? (run l/eol "\n")))
  (is (thrown? RuntimeException (run l/eol "")))
  (is (nil? (run l/eol* "\n")))
  (is (nil? (run l/eol* "      \n")))
  (is (nil? (run l/eol* "  ! hey    \n")))
  (is (nil? (run l/eol* "")))

  ;; Lines
  (is (= \0 (run (l/line-of (digit)) "0\n")))
  (is (= \0 (run (l/line-of (digit)) "0  \n")))
  (is (= \0 (run (l/line-of (digit)) "  0\n")))
  (is (= \0 (run (l/line-of (digit)) "  0  \n")))
  (is (= \0 (run (l/line-of (digit)) "0 ! hey\n")))
  (is (= \0 (run (l/line-of (digit)) "0 ! hey \n")))
  (is (= \0 (run (l/line-of (digit)) "  0 ! hey\n")))
  (is (= \0 (run (l/line-of (digit)) "  0 ! hey \n")))
  (is (= \0 (run (l/line-of (digit)) "  0 !hey\n")))
  (is (= \0 (run (l/line-of (digit)) "  0 !hey \n")))
  (is (= \0 (run (l/line-of (digit)) "  0 !hey\n")))

  ;; Blank lines
  (is (= nil (run (l/line-of l/optws) "

")))
  )

(deftest test-comments
  (is (nil? (run l/comment "      ! sup mang"))))
