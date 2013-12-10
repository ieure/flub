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
        [flub.parser.literals]))

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
