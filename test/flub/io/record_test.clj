;; -*- coding: utf-8 -*-
;;
;; © 2014, 2015 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.io.record-test
  (:use [clojure.test]
        [clojure.pprint])
  (:require [flub.io.record :as r]
            [flub.io.hex :as hex]
            [clojure.java.io :as io]))

(deftest test-pp                        ; Postprocessing
  (is (= [[:forcing-lines-available {:WAIT 32, :BUSRQ 16}]
          [:forcing-lines-enabled [:WAIT :BUSRQ]]]
         (r/pp [[:forcing-lines 48]
                [:forcing-lines-available 48]
                [:forcing-line-names-lsb ["" "" "" ""]]
                [:forcing-line-names-msb ["BUSRQ" "WAIT" "" ""]]]))))



(deftest test-trap
  (is (= [:trap {:trap-illegal-address true,
                 :trap-bad-power-supply true,
                 :trap-data-err true,
                 :trap-addr-err true,
                 :trap-ctl-err true,
                 :trap-active-force-line false,
                 :trap-active-interrupt false}]
         (r/bytes->tree [0x01 0xE7]))))

(deftest test-beep-on-error
  (is (= [:beep-on-error true] (r/bytes->tree [0x03 0x01])))
  (is (= [:beep-on-error false] (r/bytes->tree [0x03 0x00]))))

(deftest test-run-uut-addr
  (is (= [:run-uut-addr 0x12345678]
         (r/bytes->tree [0x06 0x34 0x12 0x78 0x56]))))

(deftest test-bus-test-addr
  (is (= [:bus-test-addr 0x12345678] (r/bytes->tree
                                          [0x05 0x34 0x12 0x78 0x56]))))

(deftest test-pod-name
  (is (= [:pod "8080"] (r/bytes->tree [12 56 48 56 48 0 0 0]))))

(deftest test-forcing-line-names
  (is (= [:forcing-line-names-lsb ["HALT" "BR/ACK" "" "INTR"]]
         (r/bytes->tree (hex/line->bytes ":0E48414C5400000042522F41434B0000000000000000494E545200000006"))))
  (is (= [:forcing-line-names-msb ["" "" "" ""]]
         (r/bytes->tree (hex/line->bytes ":0F000000000000000000000000000000000000000000000000000000000F")))))

(deftest test-address-descriptor
  (is (= [:ram 0 16383] (r/bytes->tree (hex/line->bytes ":19000000000000FF3F0200000000000000000059"))))
  (is (= [:ram 16384 32767] (r/bytes->tree (hex/line->bytes ":19000000400000FF7F02000000000000000000D9"))))
  (is (= [:ram 32768 49151] (r/bytes->tree (hex/line->bytes ":19000000800000FFBF0200000000000000000059"))))
  (is (= [:rom 53248 55295 1723334656] (r/bytes->tree (hex/line->bytes ":19000000D00000FFD7030000000000B8660000E0"))))
  (is (= [:rom 55296 57343 2210660352] (r/bytes->tree (hex/line->bytes ":19000000D80000FFDF030000000000C483000019"))))
  (is (= [:rom 57344 59391 3323199488] (r/bytes->tree (hex/line->bytes ":19000000E00000FFE703000000000014C60000BC"))))
  (is (= [:rom 59392 61439 190447616] (r/bytes->tree (hex/line->bytes ":19000000E80000FFEF0300000000005A0B000057"))))
  (is (= [:rom 61440 63487 1313210368] (r/bytes->tree (hex/line->bytes ":19000000F00000FFF7030000000000464E000096"))))
  (is (= [:rom 63488 65535 1448607744] (r/bytes->tree (hex/line->bytes ":19000000F80000FFFF03000000000058560000C0")))))

(deftest test-bytes->tree
  (testing "68000.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/68000.H")))))

  (testing "764977-1.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/764977-1.H")))))

  (testing "9010TEST.H"
    ;; Bad checksum
    (is (thrown? java.io.IOException
    (doall (map r/bytes->tree
                (hex/file->bytes (io/resource "fluke-hex/9010TEST.H")))))))

  (testing "APPLE-II.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/APPLE-II.H")))))

  (testing "BEAST.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/BEAST.H")))))

  (testing "CLEARPIT.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/CLEARPIT.H")))))

  (testing "GALAGA.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/GALAGA.H")))))

  (testing "GUIDEDF.H"
    (is (thrown? java.io.IOException
                 (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/GUIDEDF.H")))))))

  (testing "ID-A-DAT.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/ID-A-DAT.H")))))

  (testing "JIM.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/JIM.H")))))

  (testing "JOKER.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/JOKER.H")))))

  (testing "PAC.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/PAC.H")))))

  (testing "POLE.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/POLE.H")))))

  (testing "SA-ASYNC.H"
    (is (thrown? java.io.IOException
                 (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/SA-ASYNC.H")))))))

  (testing "Z80.H"
    (doall (map r/bytes->tree (hex/file->bytes (io/resource "fluke-hex/Z80.H"))))))
