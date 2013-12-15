;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.cond
  (:refer-clojure :exclude [char comment read])
  (:use [the.parsatron]
        [flub.parser.common]
        [flub.parser.lines]
        [flub.parser.literals]
        [flub.parser.expr]
        [flub.parser.term]
        [flub.parser.statements]))

(def ifcond
  (let->> [_ (string "IF")
           l (>> reqws expr)
           comp (>> optws (apply choice (string* "=" ">" ">=")))
           r (>> optws expr)
           t (>> reqws goto)]
          (always [:if [(kw comp) l r] t])))
