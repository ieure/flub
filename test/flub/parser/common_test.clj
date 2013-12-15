;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.common-test
  (:refer-clojure :exclude [char])
  (:use [clojure.test]
        [the.parsatron :only [run char >>]])
  (:require [flub.parser.common :as c]))

(deftest test-mchoice
  (is (= \A (run (c/mchoice (char \A) (char \B)) "A")))
  (is (= \B (run (c/mchoice (char \A) (char \B)) "B")))
  (is (thrown? RuntimeException (run (c/mchoice (char \A) (char \B)) "0")))
  (is (= \A (run (c/mchoice (>> (char \A) (char \A))
                            (>> (char \A) (char \B)))
                 "AA")))
  (is (= \B (run (c/mchoice (>> (char \A) (char \A))
                            (>> (char \A) (char \B)))
                 "AB"))))

(deftest test-mchoice*
  (is (= [] (run (c/mchoice* (>> (char \A) (char \A))
                             (>> (char \A) (char \B))) "")))
  (is (= '(\A \A \A) (run (c/mchoice* (>> (char \A) (char \A))
                                      (>> (char \A) (char \B)))
                          "AAAAAA")))
  (is (= '(\B \B \B) (run (c/mchoice* (>> (char \A) (char \A))
                                      (>> (char \A) (char \B)))
                 "ABABAB"))))
