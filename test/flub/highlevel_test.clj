;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.highlevel-test
  (:use [clojure.test]
        [clojure.pprint])
  (:require [clojure.java.io :as io]
            [flub.io.hex :as hex]
            [flub.io.record :as rec]
            [flub.parser.source :as parse]
            [flub.parser.asm :as asm]))

(deftest test-include
  (is
   (=
    [:S
     [:POD
      [:FORCELN [:FORCE_LINE "BUSRQ"] [:DEC "4"]]
      [:FORCELN [:FORCE_LINE "WAIT"] [:DEC "5"]]
      [:BUS_ADDR [:HEX "FFFF"]]
      [:UUT_ADDR [:HEX "0000"]]]
     [:PROGRAM
      [:PROGRAM_HEAD [:SYMBOL "MAIN"]]
      [:PROGRAM_BODY
       [:STATEMENT [:LABEL [:HEX "1"]]]
       [:STATEMENT
        [:REG_ASSIGN [:REGISTER "E"] [:EXPR [:TERM [:HEX "10"]]]]]
       [:STATEMENT [:LABEL [:HEX "2"]]]
       [:STATEMENT
        [:WRITE
         [:ADDR [:EXPR [:TERM [:HEX "4040"]]]]
         "="
         [:EXPR [:TERM [:REGISTER "E"]]]]]
       [:STATEMENT
        [:WRITE
         [:ADDR [:EXPR [:TERM [:HEX "405F"]]]]
         "="
         [:EXPR [:TERM [:REGISTER "E"]]]]]
       [:STATEMENT
        [:WRITE
         [:ADDR [:EXPR [:TERM [:HEX "43A0"]]]]
         "="
         [:EXPR [:TERM [:REGISTER "E"]]]]]
       [:STATEMENT
        [:WRITE
         [:ADDR [:EXPR [:TERM [:HEX "43BF"]]]]
         "="
         [:EXPR [:TERM [:REGISTER "E"]]]]]
       [:STATEMENT [:UNARY "INC" [:REGISTER "E"]]]
       [:STATEMENT
        [:IF
         [:EXPR [:TERM [:REGISTER "E"]]]
         "="
         [:EXPR [:TERM [:HEX "17"]]]
         [:GOTO [:HEX "1"]]]]
       [:STATEMENT [:GOTO [:HEX "2"]]]]]]
    (parse/source->ast (slurp (io/resource "fluke-src/PAC.S"))))))

#_(deftest test-assemble-disassemble
  (let [ast (parse/source->ast (slurp (io/resource "fluke-src/PAC.S")))
        _ (do (pprint ast))
        bytes-out (asm/ast->bytes ast)
        ;; _ (do (pprint bytes-out))
        hex-out (hex/bytes->str bytes-out)
        back-in (hex/str->bytes hex-out)
        disassembled (rec/disass back-in)]
    (println "---------- AST")
    (pprint ast)
    (println "---------- BYTES-OUT")
    (pprint bytes-out)
    (println "---------- HEX-OUT")
    (println hex-out)
    (println "---------- BACK-IN")
    (pprint back-in)
    (println "---------- DISASSEMBLED")
    (pprint disassembled)))