;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron])
  (:import [the.parsatron Continue]))



 ;; Unary operators

(def unary-operator
  (let->> [op (choice (string "CPL")
                      (string "DEC")
                      (string "INC")
                      (string "SHL")
                      (string "SHR"))
           target (either sym register)]
          (always [(kw op) target])))

 ;; Expressions

(defparser term-unary-operator-arg [op]
  (let->> [arg (>> optws decimal-constant)]
          (always [(kw op) (or arg 1)])))

(def term-unary-operator-has-arg?
  (let->> [n (either (attempt (>> optws decimal-constant))
                     (always nil))]
          (always n)))

(def term-unary-operator
  (let->> [op (apply mchoice (string* "CPL" "DEC" "INC" "SHR"))
           arg? (lookahead term-unary-operator-has-arg?)]
          (if-not (nil? arg?)
            (term-unary-operator-arg op)
            (always [(kw op) 1]))))

(def term-has-unop?
  (let->> [op? (lookahead (apply mchoice (string* "CPL" "DEC" "INC" "SHR")))]
          (always (empty? op?))))

(defparser term-unop [term]
  (let->> [ops (many1 (>> reqws term-unary-operator))]
          (always [:term term ops])))

(def term
  (let->> [target (choice register sym hex-constant)
           cont? term-has-unop?
           #_(many (>> reqws term-unary-operator))
           ]
          (if cont? (term-unop target)
              [:term target])))

(def expression
  (mchoice sym hex-constant register))

(def combining
  (let->> [comb (choice (string "AND") (string "OR"))
           r (>> reqws term)]
          (always [(kw comb) r])))

(def duop*
  (>> term reqws (string* "OR" "AND"))
  #_(let->> [l term
           op (string* "OR" "AND")
           ;; r (>> reqws term)
           ]
          (always :sup)))

(def expr
  (let->> [l term
           c (attempt (>> reqws combining))]
          (always l)))

 ;; Tests
(def comparison )

 ;; Directives

(def include
  (let->> [_ (>> (string "INCLUDE") reqws)
           file fluke-string]
          (always [:include file])))


 ;; Statements

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
           expr expression]
          (always [:assign dest expr])))

(defparser wc
  ;; Wildcard; matches `*', defaulting to `default'.
  [default]
  (let->> [_ (char \*)]
          (always default)))

(def write
  (let->> [_ (either (attempt (string "WRITE")) (string "WR"))
           _ (choice (>> optws (attempt (char \@)) optws) reqws)
           dest (choice hex-constant (wc [:register 15]) register)
           _ (>> optws (char \=) optws)
           value (either (wc [:register 14]) expression)]
          (always [:write dest value])))

#_(def read
  (let->> [_ (either (attempt (string "READ")) (string "WR"))
           _ (choice (>> optws (attempt (char \@)) optws) reqws)
           dest (choice hex-constant (wc [:register 15]) register)
           _ (>> optws (char \=) optws)
           value (either (wc [:register 14]) expression)]
          (always [:write dest value])))

 ;; Conditionals

(def conditional
  (let->> [_ (string "IF")
           a (>> reqws expression)
           comp (>> reqws (apply mchoice (string* "=" ">" ">=")))
           b (>> reqws expression)
           target (>> reqws (string "GOTO") (either sym hex-constant))]
          (always [:if [comp a b] :goto target])))

(def label
  (let->> [name (choice sym hex-constant)
           _ (char \:)]
          (always [:label name])))

(def statement
  (mchoice display
           execute
           register-assign
           label
           write))

 ;; Programs

(def program-decl
  (let->> [decl (>> (string "PROGRAM") reqws)
           name (choice decimal-constant sym)
           ;; FIXME - (attempt) doesn't work here for some reason.
           _ (many (>> reqws decimal-constant reqws (string "BYTES")))]
          (always name)))

(def program
  (let->> [name (line-of program-decl)
           body (many1 (line-of statement))]
          (always [:program name body])))

(def fluke-source
  (many (mchoice
         (line-of optws)
         (line-of include)
         setup
         address-space
         program)))


#_(>> (attempt (many (line-of optws)))
      (attempt (line-of include))
      (attempt (many (line-of optws)))
      (attempt setup)
      (attempt (many (line-of optws)))
      (attempt address-space)
      (attempt (many (line-of optws)))
      ;; (attempt global-declarations)
      (many1 program)
      (attempt (many (line-of optws))))

 ;; High-level interface

(defn parse-string [^String s]
  (remove nil? (run fluke-source s)))
