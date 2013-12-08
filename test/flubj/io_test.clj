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
  (is (=
       '([0x01 0xE7 0xE8]
         [0x02 0x0B 0x0D]
         [0x0D 0x0B 0x18]
         [0x0E 0x48 0x41 0x4C 0x54 0x00 0x00 0x00 0x42 0x52 0x2F 0x41 0x43
          0x4B 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x49 0x4E 0x54 0x52
          0x00 0x00 0x00 0x06]
         [0x0F 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00
          0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00
          0x00 0x00 0x00 0x0F]
         [0x05 0x00 0x01 0xFE 0x0F 0x13]
         [0x06 0x00 0xF6 0x00 0x00 0xFC]
         [0x1A 0x00 0x1A]
         [0x53 0x3C 0x0A 0x44 0x0F 0x00 0x1C 0x2B 0x01 0x20 0x38 0x0F 0x1C
          0x0F 0x0F 0x0F 0x0F 0x1C 0x39 0x2D 0x38 0x00 0x30 0x04 0x00
          0x00 0x00 0x00 0x00 0x00 0x2E 0x01 0x2C 0x02 0x1F 0x38 0x0F
          0x1C 0x39 0x2D 0x38 0x00 0x30 0x04 0x00 0x00 0x00 0x00 0x00
          0x00 0x2E 0x01 0x2C 0x02 0x2C 0x03 0x2B 0x02 0x3E 0xA3 0xA4
          0xC6 0x74 0x28 0x2C 0x03 0x2B 0x03 0x33 0x0F 0x33 0x0F 0x33
          0x0F 0x33 0x0F 0x33 0x0F 0x33 0x0F 0x33 0x0F 0x33 0x0F 0x33
          0x0F 0x33 0x0F 0x33 0x0F 0x33 0x0F 0x34 0x0F 0x32 0x0F 0x32
          0x0F 0x32 0x0F 0x32 0x0F 0x32 0x0F 0x32 0x0F 0x32 0x0F 0x32
          0x0F 0x32 0x0F 0x32 0x0F 0x32 0x0F 0x32 0x0F 0x3E 0xA4 0xC6
          0x74 0x2D 0x38 0x0F 0x2E 0x0E 0x0F 0x0F 0x0F 0x0F 0x0F 0x0F
          0x2C 0x04 0x2C 0x01 0x2B 0x04 0x3E 0xA3 0x74 0x3E 0xCF 0xD5
          0xD4 0xA0 0xCF 0xC6 0xA0 0xC1 0xC4 0xC4 0xD2 0xC5 0xD3 0xD3
          0xA0 0xD2 0xC1 0xCE 0xC7 0xC5 0x74 0x28 0x50 0x01 0x09 0x00
          0x02 0x3A 0x00 0x03 0x44 0x00 0x04 0x8B 0x00]
          [0x00])
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
