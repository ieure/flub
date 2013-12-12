;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.statements
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron]
        [flub.parser.common]
        [flub.parser.lines]
        [flub.parser.literals]
        [flub.parser.expr]
        ))

(def display
  (let->> [_ (>> (string "DPY") (token #{\space \tab \-}))
           msg (many1 (choice (letter) (digit)
                              (token #{\; \@ \= \< \> \' \. \? \# \+
                                       \- \, \% \* \\ \/ \" \$ \space
                                       \_})))]
          (always [:display (apply str msg)])))

(def execute
  (let->> [_ (>> (choice (attempt (string "EXECUTE PROGRAM"))
                         (attempt (string "EXECUTE"))
                         (string "EX")) reqws)
           tgt (either decimal-constant sym)]
          (always [:execute tgt])))

(def register-assign
  (let->> [dest (choice register sym)
           _ (>> optws (char \=) optws)
           ex expr]
          (always [:assign dest expr])))

(defparser wc
  ;; Wildcard; matches `*', defaulting to `default'.
  [default]
  (let->> [_ (char \*)]
          (always default)))

(def read
  (let->> [_ (guard (either (string "READ") (string "RD")))
           _ (choice (>> optws (attempt (char \@)) optws) reqws)
           src (choice hex-constant (wc [:register 15]) register)]
          (always [:read src])))

(def write
  (let->> [_ (guard (either (string "WRITE") (string "WR")))
           _ (choice (>> optws (attempt (char \@)) optws) reqws)
           dest (choice hex-constant (wc [:register 15]) register)
           _ (>> optws (char \=) optws)
           value (either (wc [:register 14]) expr)]
          (always [:write dest value])))
