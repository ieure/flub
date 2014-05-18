;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.source
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :as walk])
  (:use [flub.io.ws]
        [clojure.pprint]
        [clojure.core.match :only [match]])
  (:import [java.io IOException]))

(def p "Fluke source parser"
  (insta/parser (slurp (io/resource "source.ebnf"))))

(defn ^String normalize
  "Normalize a source input string."
  [^String inp]
  (-> (normalize-newlines inp)
      (string/upper-case)))

(declare pp-include)

(def ^:dynamic *stack* [])

(defn source->ast "Parse input and return an AST" [^String inp]
  (with-meta
    (->> (normalize inp)
         (p)
         #_(pp-include))
    {:input nil}))

(defn file->ast [file]
  (printf "Parsing `%s'\n" file)
  (binding [*stack* (conj *stack* file)]
    (with-open [i (io/reader file)]
      (with-meta
        (doall (source->ast (slurp i)))
        {:input file}))))

 ;; Includes

(defn- resource-include "Find an include file in the resources."
  [name] (io/resource (str "include/" name)))

(defn- pwd-include "Find an include file in the PWD." [name]
  (let [cwdf (io/as-file (str "./" name))]
    (when (.exists cwdf) cwdf)))

(defn- find-include "Locate an include file." [name]
  (printf "Locating include: `%s'\n" name)
  (if-let [f (first (filter identity [(resource-include name)
                                      (pwd-include name)]))]
    (do
      (printf "Found: `%s' -> `%s'\n" name f)
      f)
    (do
      (printf "Failed to find: `%s'\n" name)
      (throw (IOException. (format "Cannot find include file `%s'" name))))))

(defn pp-include "Splice included files into the AST." [ast]
  (walk/prewalk
   (fn [form]
     (match form
            [:INCLUDE name] (file->ast (find-include name))
            :else form))
   ast))
