;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.core
  (:require [flub.parser.source :as sp]
            [flub.sig :as sig])
  (:use [flub.io.mmap :only [mmap]])
  (:gen-class))

(defn sig [files]
  (doseq [file files]
    (printf "%s - %04X\n" file (sig/sign (mmap file)))))

(defn cc [files]
  (doseq [file files]
    (sp/source->ast (slurp file))
    (printf "Parsed `%s'\n" file)))

(defn -main [cmd & args]
  (condp = cmd
    "sig"     :>> (sig args)
    "compile" :>> (cc args))
  0)
