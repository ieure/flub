;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flubj.parser.setup-test
  (:use [clojure.test]
        [the.parsatron :only [run char >>]])
  (:require [flubj.parser.setup :as s]))


(deftest test-trap
  (is (= {:data-error true} (run s/trap "TRAP DATA ERROR YES")))
  (is (thrown? RuntimeException
               (run s/trap "   TRAP CONTROL ERROR YES  \n")))
  (is (= {:control-error true}
         (run s/trap "TRAP CONTROL ERROR YES"))))

(deftest test-ram
  (is (= [:ram 0 65535] (run s/ram "RAM 0000-FFFF")))
  (is (= [:ram 0 65535] (run s/ram "RAM @ 0000-FFFF")))
  (is (= [:ram 0 65535] (run s/ram "RAM @0000-FFFF"))))

(deftest test-rom
  (is (= [:rom 0 65535 4660] (run s/rom "ROM 0000-FFFF SIG 1234")))
  (is (= [:rom 0 65535 4660] (run s/rom "ROM @ 0000-FFFF   SIG 1234")))
  (is (= [:rom 0 65535 4660] (run s/rom "ROM @0000-FFFF SIG 1234"))))

(deftest test-io
  (is (= [:io 0 65535 127] (run s/io "I/O 0000-FFFF BITS 7F")))
  (is (= [:io 0 65535 255] (run s/io "I/O @ 0000-FFFF   BITS FF")))
  (is (= [:io 0 65535 10] (run s/io "I/O @0000-FFFF BITS 0A"))))

(deftest test-address-space
  (is (= [:address-space {:ram [[0 8191] [12288 12799]],
                          :rom [[8192 12287 4660]]}]
         (run s/address-space "ADDRESS SPACE\nRAM @ 0000-1FFF\nRAM 3000-31FF\nROM @ 2000-2FFF SIG 1234")))
  (is (thrown? RuntimeException
               (run s/address-space "ADDRESS SPACE INFORMATION\n"))))

(deftest test-setup
  (is (= [:setup {:active-interrupt false}]
         (run s/setup "SETUP\nTRAP ACTIVE INTERRUPT NO")))
  (is (= [:setup nil] (run s/setup "SETUP INFORMATION !hi")))
  (is (= [:setup {:active-interrupt false
                  :data-error true}]
         (run s/setup "SETUP\n    TRAP ACTIVE INTERRUPT NO\n  TRAP DATA ERROR YES")))

  (is (= [:setup {:active-force-line false
                  :active-interrupt false}]
         (run s/setup "SETUP

   TRAP ACTIVE FORCE LINE NO
   TRAP ACTIVE INTERRUPT NO
"))))
