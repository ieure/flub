;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.io.ws "Whitespace tools"
  (:require [clojure.string :as string]))

(def ^:constant eol #"(\r\n|\n|\r)")

(defn ^String normalize-newlines
  "Return input with any newline sequences converted to UNIX newlines."
  [^String inp] (string/replace inp eol "\n"))

(defn ^String truncate-eof "Strip EOF chars"
  [^String inp]
  (string/replace inp #"+" ""))

(defn ^String normalize
  "Normalize a source input string."
  [^String inp]
  (-> (normalize-newlines inp)
      (truncate-eof)
      (string/upper-case)))
