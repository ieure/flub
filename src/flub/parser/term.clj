;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.term
  "Term parsers."
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron]
        [flub.parser.common]
        [flub.parser.lines]
        [flub.parser.literals])
  (:import [the.parsatron Continue]))

(def optarget (choice register sym hex-constant))

 ;; Unary operators

(defparser unop-arg [op]
  (let->> [arg (>> optws decimal-constant)]
          (always [(kw op) (or arg 1)])))

(def ^:private unop-arg?
  (let->> [n (guard (>> optws decimal-constant))]
          (always n)))

(def ^:private unop* (apply mchoice (string* "CPL" "DEC" "INC" "SHR")))

(def unop
  (let->> [op unop*
           arg? (lookahead unop-arg?)]
          (if-not (nil? arg?)
            (unop-arg op)
            (always [(kw op) 1]))))



(defn unops
  ([] (unops []))
  ([ops]
     (cont (let->> [op (guard (>> reqws unop))]
                   (if op
                     (unops (conj ops op))
                     (always ops))))))

(def unop-term "Parse a term with one or more unary operators."
  (let->> [tgt optarget
           ops (unops)]
          (always [:term tgt ops])))



(def bare-term "Parse a term without any unary operators."
  (let->> [tgt optarget]
          (always [:term tgt])))

(def has-unop?
  (let->> [op? (lookahead (guard unop*))]
          (always (not (empty? op?)))))

(def term
  (let->> [unop? (lookahead (guard (>> optarget reqws has-unop?)))]
          (if unop? unop-term bare-term)))
