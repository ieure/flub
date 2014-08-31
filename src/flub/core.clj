;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Ian Eure.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [taoensso.timbre :as log]
            [flub.sig :as sig]
            [flub.io.hex :as hex]
            [flub.commands.compile :as compiler])
  (:use [clojure.pprint]
        [flub.macro]
        [flub.io.mmap :only [mmap]]
        [flub.io.record :only [disass]])
  (:gen-class))

(def ^:const core-options "Options which apply to all commands."
  [["-v" nil "Verbosity"
    :id :verbosity
    :assoc-fn (fn [m k _] (update-in m [k] inc))
    :default 0]])

(defn sig [files]
  (doseq [file files]
    (printf "%s - %04X\n" file (sig/sign (mmap file)))))

(defn dc [files]
  (doseq [file files]
    (pprint (disass (hex/file->bytes file))))
  0)

 ;; Commands

(declare help)

(def ^:constant command-defs
  [[sig :signature]
   [compiler/run :compile :cc :cpl]
   [dc :decompile :dc]
   [help :help]])

(defn stem
  "Create a seq of stems of name.

   ex (stem \"foo\") -> (\"foo\" \"fo\" \"f\")"

  [name]
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

(defn cmdf "Return the function for command named n"
  [n]
  (or (get commands n) (unknown-command n)))



(defn -main [& cmdargs]
  (let [{:keys [:options :arguments :errors]} (parse-opts cmdargs core-options)
        [cmd & cmdargs] arguments]
    (condp = (:verbosity options)
      4 (log/set-level! :trace)
      3 (log/set-level! :debug)
      2 (log/set-level! :info)
      1 (log/set-level! :warn)
      0 (log/set-level! :fatal)
      true nil)

    (System/exit
     (if errors
       (do (print errors) -1)
       (do1 (apply (cmdf cmd) cmdargs)
            (shutdown-agents))))))
