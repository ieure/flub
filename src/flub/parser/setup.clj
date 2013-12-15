;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.setup
  "Parsers for setup declarations."
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron]
        [flub.parser.lines]
        [flub.parser.literals]
        [flub.parser.common]))

(def trap
  (let->> [decl (string "TRAP")
           _ reqws
           trap-type (apply mchoice (string* "BAD POWER SUPPLY"
                                             "ILLEGAL ADDRESS"
                                             "ACTIVE FORCE LINE"
                                             "ACTIVE INTERRUPT"
                                             "CONTROL ERROR"
                                             "ADDRESS ERROR"
                                             "DATA ERROR"))
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

(def include
  (let->> [_ (>> (string "INCLUDE") reqws)
           file fluke-string]
          (always [:include file])))
