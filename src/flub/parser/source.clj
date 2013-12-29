;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.source
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def ^:constant eol #"(\r\n|\n|\r)")

(def p (insta/parser (slurp (io/resource "source.ebnf"))))

(defn normalize-newlines [inp]
  (string/replace inp eol "\n"))

(defn source->ast [inp]
  (p (normalize-newlines inp)))
