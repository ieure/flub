;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.assembler.symbols-test
  (:use [clojure.test]
        [clojure.pprint]
        [flub.test-util])
  (:require [flub.assembler.symbols :as syms]
            [flub.parser.source :as src]))

(deftest resolve-syms
  (is (= [:S [:INC [:REGISTER "A"]]]
         (#'syms/resolve-syms
          [:S
           [:DECLARATIONS
            [:DECL_ASSIGN [:REGISTER "A"] [:SYMBOL "CNTR"]]
            [:DECL_ASSIGN [:REGISTER "2"] [:SYMBOLS [:SYMBOL "LEFT"]
                                           [:SYMBOL "RIGHT"]]]]
           [:INC [:SYMBOL "CNTR"]]]))))

(deftest process
    (let [code "
DECLARATIONS
    ASSIGN REGB TO FOO

PROGRAM 1
DECLARATIONS
    ASSIGN REGA TO FOO

    FOO = 1
    REGA = 1

PROGRAM 2
    FOO = 2
    REGB = 2
"]
      (is (parsed-to [:S
                      [:PROGRAM
                       [:PROGRAM_HEAD [:DEC "1"]]
                       [:PROGRAM_BODY
                        ;; Both statements use REGA (= program defs override global)
                        [:STATEMENT
                         [:REG_ASSIGN [:REGISTER "A"] [:EXPR [:TERM [:HEX "1"]]]]]
                        [:STATEMENT
                         [:REG_ASSIGN [:REGISTER "A"] [:EXPR [:TERM [:HEX "1"]]]]]]]
                      [:PROGRAM
                       [:PROGRAM_HEAD [:DEC "2"]]
                       [:PROGRAM_BODY
                        ;; Both statements use REGB (= program defs are scoped)
                        [:STATEMENT
                         [:REG_ASSIGN [:REGISTER "B"] [:EXPR [:TERM [:HEX "2"]]]]]
                        [:STATEMENT
                         [:REG_ASSIGN [:REGISTER "B"] [:EXPR [:TERM [:HEX "2"]]]]]]]]
                     (syms/process (src/p code))))))
