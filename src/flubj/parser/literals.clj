;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flubj.parser.literals
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron]
        [flubj.parser.common]
        [flubj.parser.lines])
  (:require [clojure.string :as cstr]))

 ;; Constants etc

(defn- chars->long  "Convert a sequence of characters to a Long."
  [chars base]
  (Long/parseLong (apply str chars) base))

(def hex-char "Match a single hexadecimal character."
  (either (digit) (token #{\A \B \C \D \E \F})))

(def hex-byte "Match a hexidecimal byte."
  (let->> [chars (times 2 hex-char)]
          (always (chars->long chars 16))))

(def hex-constant "Match a hexidecimal constant."
  (let->> [chars (many1 hex-char)]
          (always (chars->long chars 16))))

(def binary-constant "Match a binary constant."
  (let->> [chars (many1 (either (char \0) (char \1)))]
          (always (chars->long chars 2))))

(def decimal-constant "Match a decimal constant."
  (let->> [chars (many1 (digit))]
          (always (chars->long chars 10))))

(def opt-decimal-constant "Match a decimal constant."
  (let->> [chars (many (digit))]
          (always (when-not (empty? chars) (chars->long chars 10)))))

(def register "Match a register reference."
  (let->> [decl (string "REG")
           regn hex-char]
          (always [:register (chars->long [regn] 16)])))

(def dash
  (let->> [_ (>> optws (char \-) optws)]
          (always nil)))

(def yes-no
  (let->> [v (either (string "YES") (string "NO"))]
          (always (if (= v "YES") true
                      false))))

(def address-range
  (let->> [start hex-constant
           _ optws
           _ (char \-)
           _ optws
           end hex-constant]
          (always [start end])))

(def sym
  (let->> [head (letter)
           tail (many1 (choice (letter) (digit) (char \_)))]
          (always (keyword (str (str head) (apply str tail))))))

(def fluke-string
  (let->> [_ (char \")
           value (many1 (choice (letter) (digit) (char \.) (char \space)))
           _ (char \")]
          (always (apply str value))))
