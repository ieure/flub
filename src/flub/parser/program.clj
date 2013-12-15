;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.program
  (:refer-clojure :exclude [char comment read])
  (:use [the.parsatron]
        [flub.parser.common]
        [flub.parser.lines]
        [flub.parser.literals]
        [flub.parser.statements]
        [flub.parser.setup]
        [flub.parser.expr]
        [flub.parser.cond]))

(def label
  (let->> [name (choice sym hex-constant)
           _ (char \:)]
          (always [:label name])))

;; FIXME woefully incomplete
(def statement
  (mchoice display
           execute
           register-assign
           label
           write
           read
           unary-operator
           ifcond
           bus-test
           goto
           stop
           run-uut
           comment
           ))

(def empty-line
  (>> (many1 (token #{\space \tab \newline})) (always nil)))

(def program-decl
  (let->> [decl (>> (string "PROGRAM") reqws)
           name (choice decimal-constant sym)
           ;; FIXME - (attempt) doesn't work here for some reason.
           _ (many (>> reqws decimal-constant reqws (string "BYTES")))]
          (always name)))

(def program
  (let->> [name (line-of program-decl)
           _ optws
           body (many1 (either (line-of statement) empty-line))]
          (always [:program name (remove nil? body)])))

(def fluke-source
  (many (mchoice
         (line-of include)
         setup
         address-space
         program)))

(defn string->ast [^String s]
  (remove nil? (run fluke-source s)))
