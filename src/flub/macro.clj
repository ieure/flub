; -*- coding: utf-8 -*-
;
; © 2014 Ian Eure
; Author: Ian Eure <ian.eure@gmail.com>
;
(ns flub.macro "Utility macros")

(defmacro do1
  "Evaluate `first` and `rest`, returning the value of `first`."
  [first & rest]
  `(let [res# ~first]
     ~@rest
     res#))
