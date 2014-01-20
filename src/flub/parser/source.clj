;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.source
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:use [flub.io.ws]))

(def p (insta/parser (slurp (io/resource "source.ebnf"))))

(defn normalize [inp]
  (-> (normalize-newlines inp)
      (string/upper-case)))

(defn source->ast [inp]
  (p (normalize inp)))
