;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flubj.io-test
  (:use [clojure.test]
        [the.parsatron :only [run]])
  (:require [flubj.io.hex :as hex]))

(deftest test-single-line-record
  ;; No checksum
  (is (= [1 231 232] (run hex/single-line-record ":01E7E8")))
  ;; With checksum
  (is (= [1 231 232] (run hex/single-line-record ":01E7E8$D0")))
  ;; With newline
  (is (= [1 231 232] (run hex/single-line-record ":01E7E8$D0
")))

  ;; Throws for invalid start char
  (is (thrown? RuntimeException (run hex/single-line-record "01E7E8")))

  ;; Throws for invalic checksum
  (is (thrown? RuntimeException (run hex/single-line-record ":01E7E8$D1")))
  ;; Throws for non-final line
  (is (thrown? RuntimeException (run hex/single-line-record ":01E7E8*D0")))

  ;; Parses the first of multiple lines
  (is (= [1 231 232] (run hex/single-line-record ":01E7E8\nFOO BAR"))))

(deftest test-multi-line-record
  (is (= [1 231 232] (run (hex/multi-line-record) ":01E7E8$D0")))

  (is (= [1 231 232 1 231 232] (run (hex/multi-line-record) ":01E7E8*D0\n:01E7E8$D0")))

  (is (thrown? RuntimeException (run (hex/multi-line-record) ":01E7E8*D0\n:01E7E8*D0"))))

(deftest test-continuation?
  (is (true? (run hex/continuation? ":01E7E8*D0")))
  (is (false? (run hex/continuation? ":01E7E8")))
  (is (false? (run hex/continuation? ":01E7E8$D0"))))

(deftest test-multi-or-single-line-record
  (is (= [1 231 232] (run hex/multi-or-single-line-record ":01E7E8")))
  (is (= [1 231 232] (run hex/multi-or-single-line-record ":01E7E8$D0")))
  (is (= [1 231 232 1 231 232] (run hex/multi-or-single-line-record
                                    ":01E7E8*D0\n:01E7E8$D0"))))

(deftest test-hex-parser
  (is (= '([1 231 232] [2 11 13] [13 11 24]
           [14 72 65 76 84 0 0 0 66 82 47 65 67 75 0 0 0 0 0 0 0 0 73
            78 84 82 0 0 0 6]
           [15 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 15]
           [5 0 1 254 15 19] [6 0 246 0 0 252] [26 0 26]
           [83 60 10 68 15 0 28 43 1 32 56 15 28 15 15 15 15 28 57 45
            56 0 48 4 0 0 0 0 0 0 46 1 44 2 31 56 15 28 57 45 56 0 48
            4 0 0 0 0 0 0 46 1 44 2 44 3 43 2 62 163 164 198 116 40 44
            3 43 3 51 15 51 15 51 15 51 15 51 15 51 15 51 15 51 15 51
            15 51 15 51 15 51 15 52 15 50 15 50 15 50 15 50 15 50 15
            50 15 50 15 50 15 50 15 50 15 50 15 50 15 62 164 198 116
            45 56 15 46 14 15 15 15 15 15 15 44 4 44 1 43 4 62 163 116
            62 207 213 212 160 207 198 160 193 196 196 210 197 211 211
            160 210 193 206 199 197 116 40 80 1 9 0 2 58 0 3 68 0 4
            139 0]
             [0])

         (run hex/hex-parser
              ":01E7E8
:020B0D
:0D0B18
:0E48414C5400000042522F41434B0000000000000000494E545200000006
:0F000000000000000000000000000000000000000000000000000000000F
:050001FE0F13
:0600F60000FC
:1A001A
:533C0A440F001C2B0120380F1C0F0F0F0F1C392D380030040000000000002E012C021F38*95
:0F1C392D380030040000000000002E012C022C032B023EA3A4C674282C032B03330F330F*7E
:330F330F330F330F330F330F330F330F330F330F340F320F320F320F320F320F320F320F*9E
:320F320F320F320F320F3EA4C6742D380F2E0E0F0F0F0F0F0F2C042C012B043EA3743ECF*59
:D5D4A0CFC6A0C1C4C4D2C5D3D3A0D2C1CEC7C5742850010900023A00034400048B00$99
:00

"))))
