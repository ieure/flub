;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.macro)

(defmacro do1 [first & rest]
  `(let [res# ~first]
     ~@rest
     res#))
