;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.source
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def p (insta/parser (slurp (io/resource "source.ebnf"))))

(defn source->ast [inp]
  (p inp))
