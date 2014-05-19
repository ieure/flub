;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Ian Eure.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.available-source-test
  (:use [clojure.test]
        [clojure.pprint]
        [flub.test-util])
  (:require [flub.parser.source :as p]))

#_(deftest test-source-issue
    (is (parsed? (p/source->ast (slurp "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/9010TEST.S")))))

(def ^:const example-dir
  "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/")

(defmacro test-example [^String example-file]
  `(testing ~example-file
     (is (~'parsed? (p/include ~example-dir
                               (p/source->ast (slurp ~(str example-dir example-file))))))))

(deftest test-available-source-2
  (p/include "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src"
             (test-example "68000.s")
             (test-example "9010A-TK80-Demo-Version30-Jun-81.S")

             ;; ISO-8859-1 hyphens in this
             #_(test-example "9010TEST.S")

             ;; "NOTE THAT THIS NEEDS EDITING..."
             #_(test-example "9010a-Workbook.S")

             (test-example "APPLE-II.S")
             (test-example "ASTEROID-1.S")

             ;; BINARY
             #_(test-example "ASYNC.S")

             (test-example "After-Burner.s")
             (test-example "BURNER.S")
             (test-example "Beast-Busters.s")
             (test-example "CHECK.S")
             (test-example "CLEARPIT.S")
             (test-example "DELAY.S")

             ;; Legit failure, program-scoped DECLARATIONS
             (test-example "DEMO.S")
             (test-example "FILE01.S")
             (test-example "FILE02.S")

             ;; Line hard wrapped -- not sure how to deal
             #_(test-example "FILE03.S")

             (test-example "FILE04.S")
             (test-example "FILE05.S")
             (test-example "FILE06.S")
             (test-example "FILE07.S")
             (test-example "GALAGA.S")
             (test-example "GuidedF.s")
             (test-example "Hangon.s")
             (test-example "ID-A-DAT.s")
             (test-example "JIM.S")
             (test-example "JOKER.S")
             (test-example "JOKPOK.S")
             (test-example "LELAND.S")
             (test-example "LOOPPIT.S")
             (test-example "LOOPPOLE.S")
             (test-example "OperationWolf.s")
             (test-example "PAC.S")
             (test-example "PLAY.S")
             (test-example "PLAY5.S")
             (test-example "POD_TEST_6502_Rev1-0.S")
             (test-example "POD_TEST_6800_Rev1-0.S")
             (test-example "POD_TEST_8080_Rev1-1.S")
             (test-example "POD_TEST_8085_Rev1_0.S")
             (test-example "POD_TEST_Z80_Rev1-1.S")
             (test-example "POLE.S")
             (test-example "POT_TEST_6802_Rev1-0.S")
             (test-example "Pac-corner-dots.s")
             (test-example "Pac-test.s")
             (test-example "RS232TST.S")

             ;; BINARY
             #_(test-example "Unknown tape.S")

             (test-example "Z80-IO.S")
             (test-example "Z80.S")
             (test-example "beast.s")
             (test-example "blit.s")
             (test-example "blit2.s")
             ;; BINARY
             #_(test-example "fluke-tape.s")
             (test-example "opwolf.s")))
