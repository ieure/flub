;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Ian Eure.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.core
  (:require [flub.parser.source :as sp]
            [flub.sig :as sig]
            [flub.io.hex :as hex]
            [flub.assembler :as asm]
            [instaparse.core :as insta]
            [instaparse.failure :as fail]
            [taoensso.timbre :as log])
  (:use [flub.io.mmap :only [mmap]]
        [flub.io.record :only [disass]]
        [clojure.pprint])
  (:gen-class))

(defn sig [files]
  (doseq [file files]
    (printf "%s - %04X\n" file (sig/sign (mmap file)))))

(defn cc [files]
  (doseq [file files]
    (let [r (sp/source->ast (slurp file))]
      (if (insta/failure? r)
        (do (fail/pprint-failure (insta/get-failure r))
            1)
        (do (printf "Parsed `%s'\n" file)
            0)))))

(defn dc [files]
  (doseq [file files]
    (pprint (disass (hex/file->bytes file))))
  0)

 ;; Commands

(declare help)

(def ^:constant command-defs
  [[sig :signature]
   [cc :compile :cc :cpl]
   [dc :decompile :dc]
   [help :help]])

(defn stem [name]
  (map #(subs name 0 %) (range (count name) 0 -1)))

(defn make-command [[f & names]]
  (apply hash-map
         (-> (into #{} (flatten (map #(stem (subs (str %) 1)) names)))
             (interleave (repeat f)))))

(defn make-commands [commands]
  (reduce #(conj %1 (apply dissoc (make-command %2) (keys %1)))
          {} commands))

(defn help [& _]
  (let [cmds (map (fn [[_ n & _]] (subs (str n) 1)) command-defs)]
    (println "Commands:\n")
    (doseq [c cmds]
      (println c))))

(defn unknown-command [cmd]
  (fn [args]
    (printf "Unknown command `%s'\n" cmd)
    (help args)))

(def ^:constant commands (make-commands command-defs))

(defn cmdf [n]
  (or (get commands n) (unknown-command n)))



(defn -main [cmd & args]
  ((cmdf cmd) args))
