;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.expr
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron]
        [flub.parser.common]
        [flub.parser.lines]
        [flub.parser.duop]
        [flub.parser.term]))

(def comb (apply mchoice (string* "AND" "OR")))

(defparser expr* [combn l]
  (let->> [r term]
          (always [:expr [(kw combn) l r]])))

(def expr
  (let->> [l term
           bool? (guard (>> reqws comb))]
          (if-not (empty? bool?) (>> reqws (expr* bool? l))
                  (always l))))
