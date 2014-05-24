;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.assembler.symbols
  (:require [clojure.walk :as walk])
  (:use [clojure.core.match :only [match]]
        [clojure.pprint]))

(defn- declarations? [ast]
  (match ast [:DECLARATIONS & _] true :else nil))

(defn strip-decls [form]
  (vec (if (coll? form) (remove declarations? form) form)))

(defn- resolve-syms [ast]
  (let [symtab (transient {})]
    (walk/postwalk
     (fn [form]
       (match form
              [:DECL_ASSIGN reg [:SYMBOL s]] (assoc! symtab [:SYMBOL s] reg)
              [:DECL_ASSIGN reg [:SYMBOLS & ss]]
              (doseq [s ss] (assoc! symtab s reg))

              ;; Substitute reference, strip decls
              :else (if (coll? form)
                      (strip-decls (or (get symtab form) form))
                      form)))
     ast)))

(defn process "Process symbols in a program source AST"
  [ast]
  (walk/postwalk
   (fn [form]
     (match form
            [:PROGRAM & _] (resolve-syms form)
            [:S & _] (resolve-syms form)
            :else form))
   ast))
