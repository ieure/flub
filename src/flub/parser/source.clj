;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.source
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [clojure.walk :as walk]
            [flub.parser.pod :as pod]
            [flub.parser.symbols :as symbols])
  (:use [flub.io.ws]
        [clojure.pprint]
        [clojure.core.match :only [match]])
  (:import [java.io IOException File]))

(def p "Fluke source parser"
  (insta/parser (slurp (io/resource "source.ebnf"))))

(declare include pp-include)

(def ^:dynamic *stack* [])

(defn source->ast "Parse input and return an AST" [^String inp]
  (with-meta
    (let [res (p (normalize inp))]
      (if (insta/failure? res)
        res
        (symbols/process (pp-include res))))
    {:input inp}))

(defn file->ast [file]
  (log/infof "Parsing `%s'\n" file)
  (flush)
  (binding [*stack* (conj *stack* file)]
    (with-open [i (io/reader file)]
      (with-meta
        (doall (source->ast (slurp i)))
        {:input file}))))

 ;; Includes

(def ^:dynamic *include-stack* "Look for include files in these directories."
  ["."])

(defmacro include
  [dir & exprs]
  (let [dir (if-not (coll? dir) [dir] dir)]
    `(binding [*include-stack* (concat *include-stack* ~dir)]
       ~@exprs)))

(defn- resource-include "Find an include file in the resources."
  [name] (io/resource (str "include/" name)))

(defn- path-include "Look for file in *include-stack*"
  [file]
  (->> *include-stack*
       ;; Build a seq of Files
       (map #(io/as-file (string/join File/separator [% file])))
       ;; Find the first that exists
       (filter (fn [^File f] (.exists f)))
       (first)))

(defn- find-include "Locate an include file." [name]
  (if-let [f (first (filter identity [(path-include name)
                                      (resource-include name)]))]
    f
    (throw (IOException. (format "Cannot find include file `%s'" name)))))

(defn pp-include "Splice included files into the AST." [ast]
  (walk/prewalk
   (fn [form]
     (match form
            [:INCLUDE name] (let [incf (find-include name)]
                              (log/tracef "Including file `%s' -> `%s'\n" name incf)
                              (if (.endsWith (string/lower-case name) ".pod")
                                          (pod/file->ast incf)
                                          (file->ast incf)))
            :else form))
   ast))
