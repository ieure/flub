;; -*- coding: utf-8 -*-
;;
;; © 2013 Ian Eure.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.source-test
  (:use [clojure.test]
        [clojure.pprint]
        [flub.test-util])
  (:require [flub.parser.source :as p]))

(deftest test-available-source
  (testing "68000.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/68000.s")))))
  (testing "9010A-TK80-Demo-Version30-Jun-81.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/9010A-TK80-Demo-Version30-Jun-81.S")))))
  (testing "9010TEST.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/9010TEST.S")))))
  (testing "9010a-Workbook.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/9010a-Workbook.S")))))
  (testing "APPLE-II.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/APPLE-II.S")))))
  (testing "ASTEROID-1.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/ASTEROID-1.S")))))
  (testing "ASYNC.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/ASYNC.S")))))
  (testing "After-Burner.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/After-Burner.s")))))
  (testing "BURNER.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/BURNER.S")))))
  (testing "Beast-Busters.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/Beast-Busters.s")))))
  (testing "CHECK.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/CHECK.S")))))
  (testing "CLEARPIT.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/CLEARPIT.S")))))
  (testing "DELAY.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/DELAY.S")))))
  (testing "DEMO.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/DEMO.S")))))
  (testing "FILE01.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/FILE01.S")))))
  (testing "FILE02.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/FILE02.S")))))
  (testing "FILE03.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/FILE03.S")))))
  (testing "FILE04.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/FILE04.S")))))
  (testing "FILE05.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/FILE05.S")))))
  (testing "FILE06.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/FILE06.S")))))
  (testing "FILE07.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/FILE07.S")))))
  (testing "GALAGA.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/GALAGA.S")))))
  (testing "GuidedF.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/GuidedF.s")))))
  (testing "Hangon.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/Hangon.s")))))
  (testing "ID-A-DAT.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/ID-A-DAT.s")))))
  (testing "JIM.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/JIM.S")))))
  (testing "JOKER.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/JOKER.S")))))
  (testing "JOKPOK.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/JOKPOK.S")))))
  (testing "LELAND.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/LELAND.S")))))
  (testing "LOOPPIT.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/LOOPPIT.S")))))
  (testing "LOOPPOLE.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/LOOPPOLE.S")))))
  (testing "OperationWolf.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/OperationWolf.s")))))
  (testing "PAC.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/PAC.S")))))
  (testing "PLAY.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/PLAY.S")))))
  (testing "PLAY5.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/PLAY5.S")))))
  (testing "POD_TEST_6502_Rev1-0.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/POD_TEST_6502_Rev1-0.S")))))
  (testing "POD_TEST_6800_Rev1-0.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/POD_TEST_6800_Rev1-0.S")))))
  (testing "POD_TEST_8080_Rev1-1.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/POD_TEST_8080_Rev1-1.S")))))
  (testing "POD_TEST_8085_Rev1_0.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/POD_TEST_8085_Rev1_0.S")))))
  (testing "POD_TEST_Z80_Rev1-1.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/POD_TEST_Z80_Rev1-1.S")))))
  (testing "POLE.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/POLE.S")))))
  (testing "POT_TEST_6802_Rev1-0.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/POT_TEST_6802_Rev1-0.S")))))
  (testing "Pac-corner-dots.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/Pac-corner-dots.s")))))
  (testing "Pac-test.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/Pac-test.s")))))
  (testing "RS232TST.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/RS232TST.S")))))
  (testing "Unknown tape.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/Unknown tape.S")))))
  (testing "Z80-IO.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/Z80-IO.S")))))
  (testing "Z80.S"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/Z80.S")))))
  (testing "beast.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/beast.s")))))
  (testing "fluke-tape.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/fluke-tape.s")))))
  (testing "opwolf.s"
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/opwolf.s")))))
  )
