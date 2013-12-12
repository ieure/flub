;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.cond
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron]
        [flub.parser.common]
        [flub.parser.lines]
        [flub.parser.literals]
        [flub.parser.expr]
        [flub.parser.term]))

(def goto
  (let->> [t (>> (string "GOTO") reqws (either (attempt sym) hex-constant))]
          (always [:goto t])))

(def ifcond
  (let->> [_ (string "IF")
           l (>> reqws expr)
           comp (>> reqws (apply choice (string* "=" ">" ">=")))
           r (>> reqws expr)
           t (>> reqws goto)]
          (always [:if [(kw comp) l r] t])))
