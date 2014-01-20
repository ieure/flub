;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.io.checksum)

(defn checksum "Compute a simple checksum of bytes."
  [bytes] (-> (reduce + bytes) (unchecked-remainder-int 256)))
