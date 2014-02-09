;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.io.hex-test
  (:use [clojure.test])
  (:require [flub.io.hex :as h]
            [clojure.java.io :as io])
  (:import [java.io IOException StringReader]))

 ;; Utility code

(deftest test-split-str-at
  (is (= ["12" "34"] (h/split-str-at 2 "1234")))
  (is (= ["123" "4567"] (h/split-str-at 3 "1234567")))
  (is (= ["" "1234567"] (h/split-str-at 0 "1234567")))
  (is (thrown? java.lang.StringIndexOutOfBoundsException (h/split-str-at 99 "1234567"))))

(deftest test-chunk
  (is (= '("12" "34") (h/chunk 2 "12345")))
  (is (= '("12" "34") (h/chunk 2 "1234")))
  (is (= '("12") (h/chunk 2 "12"))))



(deftest test-split-checksum
  (is (= ["0230" "32"] (h/split-checksum "023032")))
  (is (= ["532B01440E01001C2B0220040004001C380E1C200400050F1C380E1C2004030A001C380E" "0A"] (h/split-checksum "532B01440E01001C2B0220040004001C380E1C200400050F1C380E1C2004030A001C380E*0A")))
  (is (= ["1C2004030B0F1C380E1C340E2D380E2F01072C012C0250010300020A00" "82"] (h/split-checksum"1C2004030B0F1C380E1C340E2D380E2F01072C012C0250010300020A00$82")))
  (is (= ["00" "00"] (h/split-checksum "00"))))

(deftest test-hex->bytes
  (is (= [0x53 0x2B 0x01 0x44 0x0E 0x01 0x00 0x1C
          0x2B 0x02 0x20 0x04 0x00 0x04 0x00 0x1C]
         (h/hex->bytes "532B01440E01001C2B0220040004001C"))))

(deftest test-line->bytes
  (is (= [0x05 0x00 0x00 0xFF 0xFF]
         (h/line->bytes ":050000FFFF03")))
  (is (thrown? IOException (h/line->bytes ":050000FFFF04"))))

(deftest test-record->bytes
  (is (= [0x05 0x00 0x00 0xFF 0xFF]
         (h/record->bytes [":050000FFFF03"])))
  (is (thrown? IOException (h/record->bytes [":050000FFFF04"]))))

(deftest test-eor?
  (is (nil? (h/eor? ":532B01440E01001C2B0220040004001C380E1C200400050F1C380E1C2004030A001C380E*0A")))
  (is (nil? (h/eor? ":1C2004030B0F1C380E1C340E2D380E2F01072C012C0250010300020A00$82")))
  (with-redefs [clojure.core/gensym #(fn [& _] true)]
    (is (h/eor? ":1C2004030B0F1C380E1C340E2D3800250010300020A0082"))))

(deftest test-hex-record?
  (is (h/hex-record? ":1234"))
  (is (not (h/hex-record? "1234"))))



(deftest test-lines->records
    (is (= '((":01EFF0") (":023032") (":0D303D")
             (":532B01440E01001C2B0220040004001C380E1C200400050F1C380E1C2004030A001C380E*0A"
              ":1C2004030B0F1C380E1C340E2D380E2F01072C012C0250010300020A00$82")
             (":1A001A") (":060000000006") (":00"))

           (#'h/lines->records
            [":01EFF0" ":023032" ":0D303D"
             ":532B01440E01001C2B0220040004001C380E1C200400050F1C380E1C2004030A001C380E*0A"
             "" ":1C2004030B0F1C380E1C340E2D380E2F01072C012C0250010300020A00$82"
             ":1A001A" "" "" ":060000000006" ":00"]))))

(deftest test-file->records
  (is (= '((":01EFF0") (":023032") (":0D303D")
           (":532B01440E01001C2B0220040004001C380E1C200400050F1C380E1C2004030A001C380E*0A"
            ":1C2004030B0F1C380E1C340E2D380E2F01072C012C0250010300020A00$82")
           (":1A001A") (":060000000006") (":00"))

         (#'h/file->records (StringReader. ":01EFF0
:023032
:0D303D
:532B01440E01001C2B0220040004001C380E1C200400050F1C380E1C2004030A001C380E*0A

:1C2004030B0F1C380E1C340E2D380E2F01072C012C0250010300020A00$82
:1A001A


:060000000006
:00
")))))

(deftest test-file->bytes
  (is (= '((1 239) (2 48) (13 48)
           (83 43 1 68 14 1 0 28 43 2 32 4 0 4 0 28 56 14 28 32 4 0 5 15
               28 56 14 28 32 4 3 10 0 28 56 14 28 32 4 3 11 15 28 56 14
               28 52 14 45 56 14 47 1 7 44 1 44 2 80 1 3 0 2 10 0)
           (26 0) (6 0 0 0 0) (0))
         (h/file->bytes (StringReader. ":01EFF0
:023032
:0D303D
:532B01440E01001C2B0220040004001C380E1C200400050F1C380E1C2004030A001C380E*0A

:1C2004030B0F1C380E1C340E2D380E2F01072C012C0250010300020A00$82
:1A001A


:060000000006
:00
")))))

(deftest test-str->bytes
  (is (= '((1 239) (2 48) (13 48)
           (83 43 1 68 14 1 0 28 43 2 32 4 0 4 0 28 56 14 28 32 4 0 5 15
               28 56 14 28 32 4 3 10 0 28 56 14 28 32 4 3 11 15 28 56 14
               28 52 14 45 56 14 47 1 7 44 1 44 2 80 1 3 0 2 10 0)
           (26 0) (6 0 0 0 0) (0))
         (h/str->bytes ":01EFF0
:023032
:0D303D
:532B01440E01001C2B0220040004001C380E1C200400050F1C380E1C2004030A001C380E*0A

:1C2004030B0F1C380E1C340E2D380E2F01072C012C0250010300020A00$82
:1A001A


:060000000006
:00
"))))

(deftest test-record->hex-dispatch
  (is (= :fixed (#'h/record->hex-dispatch [1 239])))
  (is (= :fixed (#'h/record->hex-dispatch [13 48])))
  (is (= :variable (#'h/record->hex-dispatch [83 43 1 68]))))

(deftest test-record->hex
  (testing "Fixed length"
    (let [bytes [13 48]
          hex (h/record->hex bytes)
          bytesp (h/record->bytes hex)]
      (is (= [":0D303D"] hex))
      (is (= bytes bytesp))))

  (testing "Variable length"
    (let [bytes '(83 43 1 68 14 1 0 28 43 2 32 4 0 4 0 28 56 14 28 32
                    4 0 5 15 28 56 14 28 32 4 3 10 0 28 56 14 28 32 4
                    3 11 15 28 56 14 28 52 14 45 56 14 47 1 7 44 1 44
                    2 80 1 3 0 2 10 0)
          hex (h/record->hex bytes)
          bytesp (h/record->bytes hex)]
      (is (= '(":532B01440E01001C2B0220040004001C380E1C200400050F1C380E1C2004030A001C380E*0A"
               ":1C2004030B0F1C380E1C340E2D380E2F01072C012C0250010300020A00$82")
             hex))
      (is (= bytes bytesp)))))

(deftest test-roundtrip
  (let [bytes (h/file->bytes (io/resource "fluke-hex/PAC.H"))]
    (is (= bytes (#'h/str->bytes (h/bytes->str bytes))))))

(deftest test-known
  (testing "68000.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/68000.H")))))

  (testing "764977-1.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/764977-1.H")))))

  (testing "9010TEST.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/9010TEST.H")))))

  (testing "APPLE-II.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/APPLE-II.H")))))

  (testing "BEAST.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/BEAST.H")))))

  (testing "CLEARPIT.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/CLEARPIT.H")))))

  (testing "GALAGA.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/GALAGA.H")))))

  (testing "GUIDEDF.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/GUIDEDF.H")))))

  (testing "ID-A-DAT.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/ID-A-DAT.H")))))

  (testing "JIM.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/JIM.H")))))

  (testing "JOKER.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/JOKER.H")))))

  (testing "PAC.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/PAC.H")))))

  (testing "POLE.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/POLE.H")))))

  (testing "SA-ASYNC.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/SA-ASYNC.H")))))

  (testing "Z80.H"
    (is (seq? (h/file->bytes (io/resource "fluke-hex/Z80.H"))))))
