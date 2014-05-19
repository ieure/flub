;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.pod
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :as walk])
  (:use [flub.io.ws]
        [clojure.pprint]
        [clojure.core.match :only [match]])
  (:import [java.io IOException]))

(def p "Fluke pod definition parser"
  (insta/parser (slurp (io/resource "pod.ebnf"))))

(defn- patch-tree [ast]
   (vec (cons :POD (rest ast))))

(defn source->ast "Parse input and return an AST" [^String inp]
  (with-meta
    (patch-tree (p (normalize inp)))
    {:input inp}))

(defn file->ast [file]
  (printf "Parsing POD `%s'\n" file)
  (with-open [i (io/reader file)]
    (with-meta
      (doall (source->ast (slurp i)))
      {:input file})))
