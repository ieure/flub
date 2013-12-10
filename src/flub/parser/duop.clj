;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.duop
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron]
        [flub.parser.common]
        [flub.parser.lines]
        [flub.parser.literals]
        [flub.parser.term]))

(def ^:private duop*
  (let->> [l term
           comb (>> reqws (apply mchoice (string* "AND" "OR")))
           r (>> reqws term)]
          (always [(kw comb) l r])))

(def duop (guard duop*))
