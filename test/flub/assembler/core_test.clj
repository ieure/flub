;; -*- coding: utf-8 -*-
;;
;; © 2014, 2015 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.assembler.core-test
  (:use [clojure.test]
        [clojure.pprint])
  (:require [flub.assembler.core :as asm]
            [flub.io.record :as r]
            [flub.io.hex :as hex]
            [flub.parser.source :as p]
            [taoensso.timbre :as log]))

(defonce ast
  [:S
   [:PROGRAM
    [:PROGRAM_HEAD [:SYMBOL "BLITUI"]]
    [:PROGRAM_BODY
     [:STATEMENT [:DISPLAY [:STRING "START ADDRESS @ /A"]]]
     [:STATEMENT [:DISPLAY [:STRING "END ADDRESS @ /B"]]]
     [:STATEMENT [:DISPLAY [:STRING "VALUE = /C"]]]
     [:STATEMENT [:EXECUTE [:SYMBOL "BLIT"]]]]]

   [:PROGRAM
    [:PROGRAM_HEAD [:SYMBOL "BLIT"]]
    [:PROGRAM_BODY
     [:STATEMENT [:LABEL [:SYMBOL "WRITE"]]]
     [:STATEMENT
      [:WRITE
       [:ADDR [:EXPR [:TERM [:REGISTER "A"]]]]
       "="
       [:EXPR [:TERM [:REGISTER "C"]]]]]
     [:STATEMENT
      [:IF
       [:EXPR [:TERM [:REGISTER "A"]]]
       ">="
       [:EXPR [:TERM [:REGISTER "B"]]]
       [:GOTO [:SYMBOL "DONE"]]]]
     [:STATEMENT [:UNARY "INC" [:REGISTER "A"]]]
     [:STATEMENT [:GOTO [:SYMBOL "WRITE"]]]
     [:STATEMENT
      [:LABEL [:SYMBOL "DONE"]]
      [:STATEMENT [:DISPLAY [:STRING "DONE"]]]]]]])

(defonce ast2
  [:S
   [:PROGRAM
    [:PROGRAM_HEAD [:DEC "0"]]
    [:PROGRAM_BODY
     [:STATEMENT [:DISPLAY [:STRING "START ADDRESS @ /A"]]]
     [:STATEMENT [:DISPLAY [:STRING "END ADDRESS @ /B"]]]
     [:STATEMENT [:DISPLAY [:STRING "VALUE = /C"]]]
     [:STATEMENT [:EXECUTE [:DEC "1"]]]]]
   [:PROGRAM
    [:PROGRAM_HEAD [:DEC "1"]]
    [:PROGRAM_BODY
     [:STATEMENT [:LABEL [:HEX "1"]]]
     [:STATEMENT
      [:WRITE
       [:ADDR [:EXPR [:TERM [:REGISTER "A"]]]]
       "="
       [:EXPR [:TERM [:REGISTER "C"]]]]]
     [:STATEMENT
      [:IF
       [:EXPR [:TERM [:REGISTER "A"]]]
       ">="
       [:EXPR [:TERM [:REGISTER "B"]]]
       [:GOTO [:HEX "2"]]]]
     [:STATEMENT [:UNARY "INC" [:REGISTER "A"]]]
     [:STATEMENT [:GOTO [:HEX "1"]]]
     [:STATEMENT
      [:LABEL [:HEX "2"]]
      [:STATEMENT [:DISPLAY [:STRING "DONE"]]]]]]])

(defonce ast3
  [:S
   [:PROGRAM
    [:PROGRAM_HEAD [:SYMBOL "BLIT"]]
    [:PROGRAM_BODY
     [:STATEMENT [:LABEL [:SYMBOL "WRITE"]]]
     [:STATEMENT
      [:WRITE
       [:ADDR [:EXPR [:TERM [:REGISTER "A"]]]]
       "="
       [:EXPR [:TERM [:REGISTER "C"]]]]]
     [:STATEMENT
      [:IF
       [:EXPR [:TERM [:REGISTER "A"]]]
       ">="
       [:EXPR [:TERM [:REGISTER "B"]]]
       [:GOTO [:SYMBOL "DONE"]]]]
     [:STATEMENT [:UNARY "INC" [:REGISTER "A"]]]
     [:STATEMENT [:GOTO [:SYMBOL "WRITE"]]]
     [:STATEMENT
      [:LABEL [:SYMBOL "DONE"]]
      [:STATEMENT [:DISPLAY [:STRING "DONE"]]]]]]])

(deftest test-vcc
  (is (= [1 2 3 4] (asm/vcc [1 2] [3 4])))
  (is (= [1 2 3 4] (asm/vcc 1 2 [3 4])))
  (is (= [1 2 3 4] (asm/vcc '(1 2) [3 4]))))

(deftest test-resolve-prog
  (testing "From sym, with state"
    (is (= 0 (asm/resolve-prog {:progs ["BLIT"]} [:SYMBOL "BLIT"]))))

  (testing "From num"
    (is (= 1 (asm/resolve-prog asm/no-state [:DEC "1"])))))

#_(deftest test-ast->bytes
  (let [bytes (asm/ast->bytes ast3)]
    (pprint bytes)
    (pprint (r/disass bytes))))

(deftest test-emit-hex
  (is (= [1 0xF 0xF 0xF] (asm/emit [:HEX "1FFF"]))))

(deftest test-emit-dec
  (is (= [1 0 0 0] (asm/emit [:DEC "1000"]))))

(deftest test-emit-display
  (is (= [62 211 212 193 210 212 160 193 196 196 210 197 211 211 160
          192 160 175 193 0 116]
         (asm/emit [:DISPLAY [:STRING "START ADDRESS @ /A"]]))))

(deftest test-emit-display-statement
  (is (= [[62 211 212 193 210 212 160 193 196 196 210 197 211 211 160
          192 160 175 193 0 116]]
         (asm/emit [:STATEMENT [:DISPLAY [:STRING "START ADDRESS @ /A"]]]))))

(deftest test-emit-register
  (is (= [56 10] (asm/emit [:REGISTER "A"]))))

(deftest test-emit-label-literal
  (is (= [43 2] (asm/emit [:LABEL [:HEX "2"]]))))

(deftest test-emit-label-sym
  (is (= [43 1] (asm/emit {:labels ["ONE" "TWO"]}
                        [:LABEL [:SYMBOL "TWO"]]))))

(deftest test-emit-addr
  (is (= [56 10] (asm/emit [:ADDR [:EXPR [:TERM [:REGISTER "A"]]]]))))

(deftest test-emit-unop
  (is (= [52                            ; INC
          10]                           ; A
       (asm/emit [:UNARY "INC" [:REGISTER "A"]]))))

(deftest test-emit-program-head-literal
  (is (= [0x1A                          ; Program start
          0x01]                         ; #1
         (asm/emit [:PROGRAM_HEAD [:DEC "1"]]))))

(deftest test-emit-program-head-symbol
  (is (= [0x1A                          ; Program start
          0x00]                         ; #0
         (asm/emit {:progs ["BLIT"]} [:PROGRAM_HEAD [:SYMBOL "BLIT"]]))))

(deftest test-make-label-table
  ;; Example from 9010A Programming Manual p. 7-6.
  (is (= '(1 7 0) (#'asm/make-label-table
                   [0x53 0x1F 0x01 0x01 0x1C 0x2B 0x01 0x1F 0x03 0x04
                    0x1C 0x50]))))

(deftest test-labels
  (testing 'scan-labels
    ;; No labels here
    (is (= []
           (asm/scan-labels (-> ast (rest) (first)))))
    ;; Two labels
    (is (= ["WRITE" "DONE"]
           (asm/scan-labels (-> ast (rest) (rest) (first))))))

  (testing 'resolve-labels
    (let [state {:labels (asm/scan-labels (-> ast (rest) (rest) (first)))}]
      (is (= 1 (asm/resolve-label state [:SYMBOL "DONE"])))
      (is (= 0 (asm/resolve-label state [:SYMBOL "WRITE"]))))))



(def ^:const example-dir
  "/Users/ieure/Dropbox/Projects/flub/examples/fluke-src/")

(defn- unhandled "Return any unhandled records"
  [bytes]
  (loop [[head & tail] (flatten bytes)
         acc #{}]
    (cond
     (nil? head) acc                    ; Done - return accumulator
     ;; Add unhandled record
     (= :FIXME head) (recur tail (conj acc (:terminal (first tail))))
     ;; Handled record, skip
     true (recur tail acc))))

(defmacro test-example [^String example-file]
  `(testing ~(str "Assembling " example-file)
     (let [ast# (p/include ~example-dir
                           (p/file->ast ~(str example-dir example-file)))
           recs# (asm/ast->bytes ast#)
           unh# (unhandled recs#)]
       (when-not (empty? unh#)
         (pprint ast#)
         (pprint recs#))
       (is (empty? unh#)
           (format "Unhandled instructions in output: %s" unh#)))))

#_(deftest test-failing-source
  #_(test-example "APPLE-II.S")
  #_(test-example "Z80.S")
  #_(test-example "beast.s")
  (test-example "opwolf.s"))

#_(deftest test-available-source
  (test-example "68000.s")
  (test-example "9010A-TK80-Demo-Version30-Jun-81.S")

  ;; ;; ISO-8859-1 hyphens in this
  ;; #_(test-example "9010TEST.S")

  ;; ;; "NOTE THAT THIS NEEDS EDITING..."
  ;; #_(test-example "9010a-Workbook.S")

  (test-example "APPLE-II.S")
  ;; (test-example "ASTEROID-1.S") ;; Throws exception resolving syms

  ;; ;; BINARY
  ;; #_(test-example "ASYNC.S")

  (test-example "After-Burner.s")
  (test-example "BURNER.S")
  (test-example "Beast-Busters.s")
  (test-example "CHECK.S")
  (test-example "CLEARPIT.S")
  (test-example "DELAY.S")
  (test-example "DEMO.S")
  (test-example "FILE01.S")
  (test-example "FILE02.S")

  ;; ;; Line hard wrapped -- not sure how to deal
  ;; #_(test-example "FILE03.S")

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

  ;; ;; BINARY
  ;; #_(test-example "Unknown tape.S")

  (test-example "Z80-IO.S")
  (test-example "Z80.S")
  (test-example "beast.s")
  (test-example "blit.s")
  (test-example "blit2.s")
  ;; ;; BINARY
  ;; #_(test-example "fluke-tape.s")
  (test-example "opwolf.s"))

(deftest test-prog-name
  (is (= "0" (#'asm/prog-name [:DEC "0"])))
  (is (= "MAIN" (#'asm/prog-name [:SYMBOL "MAIN"]))))
