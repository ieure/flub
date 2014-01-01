;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.record-io-test
  (:use [clojure.test]
        [clojure.pprint]
        [the.parsatron :only [run]])
  (:require [flub.io.record :as rec]
            [flub.io.hex :as hex]))

(deftest test-trap
  (is (= [:trap {:trap-illegal-address true,
                 :trap-bad-power-supply true,
                 :trap-data-err true,
                 :trap-addr-err true,
                 :trap-ctl-err true,
                 :trap-active-force-line false,
                 :trap-active-interrupt false}]
         (run rec/record [0x01 0xE7]))))

(deftest test-forcing-lines-mask)

(deftest test-beep-on-error
  (is (= [:beep-on-error true] (run rec/record [0x03 0x01])))
  (is (= [:beep-on-error false] (run rec/record [0x03 0x00]))))

(deftest test-run-uut-addr
  (is (= [:run-uut-addr 0x12345678]
         (run rec/record [0x06 0x34 0x12 0x78 0x56]))))

(deftest test-bus-test-addr
  (is (= [:bus-test-addr 0x12345678] (run rec/record
                                             [0x05 0x34 0x12 0x78 0x56]))))

(deftest test-pod-name
  (is (= [:pod "8080"] (run rec/record [12 56 48 56 48 0 0 0]))))

(deftest test-forcing-line-names
  (is (= [:forcing-line-names-1 ["HALT" "BR/ACK" "" "INTR"]]
         (run rec/record (hex/str->bytes ":0E48414C5400000042522F41434B0000000000000000494E545200000006"))))
  (is (= [:forcing-line-names-2 ["" "" "" ""]]
         (run rec/record (hex/str->bytes ":0F000000000000000000000000000000000000000000000000000000000F")))))

(deftest test-address-descriptor
  (is (= [:ram 0 16383] (run rec/record (hex/str->bytes ":19000000000000FF3F0200000000000000000059"))))
  (is (= [:ram 16384 32767] (run rec/record (hex/str->bytes ":19000000400000FF7F02000000000000000000D9"))))
  (is (= [:ram 32768 49151] (run rec/record (hex/str->bytes ":19000000800000FFBF0200000000000000000059"))))
  (is (= [:rom 53248 55295 1723334656] (run rec/record (hex/str->bytes ":19000000D00000FFD7030000000000B8660000E0"))))
  (is (= [:rom 55296 57343 2210660352] (run rec/record (hex/str->bytes ":19000000D80000FFDF030000000000C483000019"))))
  (is (= [:rom 57344 59391 3323199488] (run rec/record (hex/str->bytes ":19000000E00000FFE703000000000014C60000BC"))))
  (is (= [:rom 59392 61439 190447616] (run rec/record (hex/str->bytes ":19000000E80000FFEF0300000000005A0B000057"))))
  (is (= [:rom 61440 63487 1313210368] (run rec/record (hex/str->bytes ":19000000F00000FFF7030000000000464E000096"))))
  (is (= [:rom 63488 65535 1448607744] (run rec/record (hex/str->bytes ":19000000F80000FFFF03000000000058560000C0")))))

(deftest test-bytes->records
  (pprint (rec/bytes->records (hex/parse-file "/Users/ieure/Dropbox/Projects/flub/68000.H")))
  )
