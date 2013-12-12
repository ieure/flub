;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.test-statements
  (:use [clojure.test]
        [the.parsatron :only [run digit >>]])
  (:require [flub.parser.statements :as p]))

(deftest test-display
  (is (= [:display "HI"] (run p/display "DPY HI")))
  (is (= [:display "HI"] (run p/display "DPY-HI")))
  (is (= [:display "- HI"] (run p/display "DPY - HI")))
  (is (= [:display "#"] (run p/display "DPY-#")))
  (is (= [:display "SILENCING SOUND"] (run p/display "DPY-SILENCING SOUND"))))

(deftest test-execute
  (is (= [:execute :DELAY] (run p/execute "EX DELAY")))
  (is (= [:execute :DELAY] (run p/execute "EXECUTE DELAY")))
  (is (= [:execute :DELAY] (run p/execute "EXECUTE PROGRAM DELAY"))))

(deftest test-write
  (is (= [:write [:register 15] [:term 0]] (run p/write "WRITE @ REGF=00")))
  (is (= [:write 3145728 [:term 0]] (run p/write "WRITE @ 300000 = 0")))
  (is (= [:write 3162111 [:term 17]] (run p/write "WRITE @ 303FFF = 11")))
  (is (= [:write 2129920 [:term 0]] (run p/write "WRITE @ 208000 = 0")))
  (is (= [:write 2131967 [:term 17]] (run p/write "WRITE @ 2087FF = 11")))
  (is (= [:write 2131968 [:term 0]] (run p/write "WRITE @ 208800 = 0")))
  (is (= [:write 2132991 [:term 17]] (run p/write "WRITE @ 208BFF = 11")))
  (is (= [:write 2134016 [:term 0]] (run p/write "WRITE @ 209000 = 0")))
  (is (= [:write 2135039 [:term 17]] (run p/write "WRITE @ 2093FF = 11")))
  (is (= [:write 2136064 [:term 0]] (run p/write "WRITE @ 209800 = 0")))
  (is (= [:write 2137087 [:term 17]] (run p/write "WRITE @ 209BFF = 11"))))
