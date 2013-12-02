;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flubj.parser
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron])
  (:require [clojure.string :as cstr]))

(defn- chars->long  "Convert a sequence of characters to a Long."
  [chars base]
  (Long/parseLong (apply str chars) base))

(defn kw  "Convert a string to a keyword."
  [s]
  (-> (cstr/lower-case s)
      (cstr/replace #" +" "-")
      (keyword)))



(defparser butchar
;;  "Match any token except `c'."
  [c]
  (token #(not= c %)))

(defn- string* [& s]
  (map string s))

(defparser mchoice
;;  "Multiple-choice; attempt each of `parsers' in turn."
  [& parsers]
  (apply choice (map attempt parsers)))

(defparser mchoice*
  ;;  "Multiple-choice; attempt each of `parsers' in turn."
  [& parsers]
  (many (apply mchoice parsers)))

(defparser mchoice+
  ;;  "Multiple-choice; attempt each of `parsers' in turn."
  [& parsers]
  (many1 (apply mchoice parsers)))

 ;; Whitespace & line handling

(def ws "Match one non-newline whitespace char. Don't use this directly."
  (>> (token #{\space \tab}) (always nil)))

(def anyws "Match any whitespace character."
  (>> (many (token #{\space \tab \newline})) (always nil)))

(def optws "Match 0 or more non-newline whitespace chars."
  (>> (many ws) (always nil)))

(def reqws "Match 1 or more non-newline whitespace characters."
  (>> (many1 ws) (always nil)))

(def comment "Match a comment at the end of a line. Don't use directly."
  (let->> [_ (>> optws (char \!) (many (butchar \newline)))]
          (always nil)))

(def eol "Match a single newline. Don't use directly."
  (>> (char \newline) (always nil)))

(def eol* "Match to EOL, including comment."
  (>> optws (many comment) (either eol (eof))))

(defparser line-of
  ;; "Match a line which contains `parser'.

  ;;  A line may contain leading and trailing whitespace, an optional
  ;;  comment, and MUST end with an EOL character."
  [parser]
  (let->> [_ optws
           r parser
           _ eol*]
          (always r)))

 ;; Constants etc

(def hex-char "Match a single hexadecimal character."
  (either (digit) (token #{\A \B \C \D \E \F})))

(def hex-constant "Match a hexidecimal constant."
  (let->> [chars (many1 hex-char)]
          (always (chars->long chars 16))))

(def binary-constant "Match a binary constant."
  (let->> [chars (many1 (either (char \0) (char \1)))]
          (always (chars->long chars 2))))

(def decimal-constant "Match a decimal constant."
  (let->> [chars (many1 (digit))]
          (always (chars->long chars 10))))

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

;; FIXME - needs number of times shortcut, but (attempt) still throws
;; an exception, which is whack.
(def term-unary-operator
  (let->> [op (apply mchoice (string* "CPL" "DEC" "INC" "SHR"))]
          (always [(kw op) 1])))

(def term
  (let->> [target (mchoice* register sym hex-constant)
           ops (attempt (many (>> reqws term-unary-operator)))]
          (always [:term (first target) ops])))

(def expression
  (mchoice hex-constant register))

(def combining
  (let->> [comb (choice (string "AND") (string "OR"))
           r (>> reqws term)]
          (always [(kw comb) r])))

#_(def expr
  (let->> [l term
           c (attempt (>> reqws combining))]
          (always l))
  )

 ;; Tests
(def comparison )

 ;; Directives

(def include
  (let->> [_ (>> (string "INCLUDE") reqws)
           file fluke-string]
          (always [:include file])))

 ;; Setup

(def trap
  (let->> [decl (string "TRAP")
           _ reqws
           trap-type (choice
                      (string "BAD POWER SUPPLY")
                      (string "ILLEGAL ADDRESS")
                      ;; Docs lie! If you TRAP ACTIVE INTERRUPT, this
                      ;; throws a parse error.
                      (attempt (string "ACTIVE FORCE LINE"))
                      (string "ACTIVE INTERRUPT")
                      (string "CONTROL ERROR")
                      (string "ADDRESS ERROR")
                      (string "DATA ERROR"))
           _ reqws
           v yes-no]
          (always {(kw trap-type) v})))

(def pod
  (let->> [_ (>> (string "POD") reqws)
           ;; fixme dash
           podn (many1 (choice (letter) (digit) (char \') (char \/)))]
          (always [:pod podn])))

(def setup
  ;; Most specific has to come first!
  (let->> [decl (line-of (either (attempt (string "SETUP INFORMATION"))
                                 (string "SETUP")))
           _ anyws
           trapdefs (many (line-of trap))]
          (always [:setup (apply merge trapdefs)])))

(def ram
  (let->> [_ (>> (string "RAM")
                 (choice (attempt (>> reqws (char \@) optws)) reqws))
           [start end] address-range]
          (always [:ram start end])))

(def rom
  (let->> [_ (>> (string "ROM")
                 (choice (attempt (>> reqws (char \@) optws)) reqws))
           [start end] address-range
           _ reqws
           sig (>> (string "SIG") reqws hex-constant)]
          (always [:rom start end sig])))

(def io
  (let->> [_ (>> (string "I/O")
                 (choice (attempt (>> reqws (char \@) optws)) reqws))
           [start end] address-range
           _ reqws
           bits (>> (string "BITS") reqws hex-constant)]
          (always [:io start end bits])))

(def address-space
  (let->> [decl (line-of (either (attempt
                                  (string "ADDRESS SPACE INFORMATION"))
                                 (string "ADDRESS SPACE")))
           spaces (many1 (line-of (choice (attempt ram) rom io)))]
          (always [:address-space
                   (->> (group-by first spaces)
                        (mapv (fn [[k v]] [k (mapv #(vec (rest %)) v)]))
                        (into {}))])))

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
