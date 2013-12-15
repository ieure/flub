;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.common
  "Common helpers for parsers."
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron])
  (:require [clojure.string :as cstr])
  (:import [the.parsatron Continue]))

(defparser butchar
  ;;  "Match any token except `c'."
  [c]
  (token #(not= c %)))

;; Might be useful?
#_(defparser optional [p]
    (either (attempt b)
            (always nil)))

(defn string* [& s]
  (map string s))

(defparser mchoice
  ;;  "Multiple-choice; attempt each of `parsers' in turn."
  [& parsers]
  (apply choice (map attempt parsers)))

(defparser mchoice*
  ;;  "Multiple-choice; attempt each of `parsers' in turn."
  [& parsers]
  (many (apply mchoice parsers)))

(defn kw "Convert a string to a keyword." [s]
  (-> (cstr/lower-case s)
      (cstr/replace #" +" "-")
      (keyword)))



(defmacro cont [p]
  `(fn [state# cok# cerr# eok# eerr#]
     (Continue. #(~p state# cok# cerr# eok# eerr#))))

(defn pdebug [p]
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (printf "%s <- %s\n" p {:input input :pos (:column pos)})
    (Continue. #(p state cok cerr eok eerr))))

(defparser guard [p]
  (either (attempt p)
          (always nil)))
