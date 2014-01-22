;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.asm-test
  (:use [clojure.test]
        [clojure.pprint])
  (:require [flub.parser.asm :as a]
            [flub.parser.source :as s]))

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
  (is (= [1 2 3 4] (a/vcc [1 2] [3 4])))
  (is (= [1 2 3 4] (a/vcc 1 2 [3 4])))
  (is (= [1 2 3 4] (a/vcc '(1 2) [3 4]))))

(deftest test-ast->bytes
  (pprint (a/ast->bytes ast3)))

(deftest test-emit-hex
  (is (= [1 0xF 0xF 0xF] (a/emit [:HEX "1FFF"]))))

(deftest test-emit-dec
  (is (= [1 0 0 0] (a/emit [:DEC "1000"]))))

(deftest test-emit-display
  (is (= [62 211 212 193 210 212 160 193 196 196 210 197 211 211 160
          192 160 175 193]
         (a/emit [:DISPLAY [:STRING "START ADDRESS @ /A"]]))))

(deftest test-emit-display-statement
  (is (= [[62 211 212 193 210 212 160 193 196 196 210 197 211 211 160
          192 160 175 193]]
         (a/emit [:STATEMENT [:DISPLAY [:STRING "START ADDRESS @ /A"]]]))))

(deftest test-emit-register
  (is (= [56 10] (a/emit [:REGISTER "A"]))))

(deftest test-emit-label-literal
  (is (= [43 2] (a/emit [:LABEL [:HEX "2"]]))))

(deftest test-emit-label-sym
  (is (= [43 1] (a/emit {:labels ["ONE" "TWO"]}
                        [:LABEL [:SYMBOL "TWO"]]))))

(deftest test-emit-addr
  (is (= [56 10] (a/emit [:ADDR [:EXPR [:TERM [:REGISTER "A"]]]]))))

(deftest test-emit-unop
  (is (= [52                            ; INC
          56                            ; REG
          10]                           ; A
       (a/emit [:UNARY "INC" [:REGISTER "A"]]))))

(deftest test-emit-program-head-literal
  (is (= [0x1A                          ; Program start
          0x01]                         ; #1
         (a/emit [:PROGRAM_HEAD [:DEC "1"]]))))

(deftest test-emit-program-head-symbol
  (is (= [0x1A                          ; Program start
          0x00]                         ; #0
         (a/emit {:progs ["BLIT"]} [:PROGRAM_HEAD [:SYMBOL "BLIT"]]))))
