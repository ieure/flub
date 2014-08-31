;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.commands.compile
  (:require [clojure.stacktrace :as stacktrace]
            [taoensso.timbre :as log]
            [flub.parser.source :as sp]
            [flub.assembler.core :as asm]
            [flub.io.hex :as hex]
            [instaparse.core :as insta]
            [instaparse.failure :as fail])
  (:use [clojure.pprint]
        [slingshot.slingshot :only [try+]])
  (:refer-clojure :exclude [compile]))

(defn ppspy [level name expr]
  (log/log level (str name "\n") (with-out-str (pprint expr)))
  expr)

(defn compile "Compile files to hex." [input]
  (try+
   (let [ast (ppspy :trace "AST" (sp/file->ast input))
         bytes (ppspy :trace "Bytes" (asm/ast->bytes ast))
         out (hex/bytes->str bytes)]
     (println out))
   0
   (catch instaparse.gll.Failure f
     (fail/pprint-failure (insta/get-failure f))
     1)
   (catch [:undefined :symbol] ude
     (printf "Undefined symbol: `%s'\n" (:symbol ude))
     2)
   (catch Exception e
     (stacktrace/print-cause-trace e)
     3)))

(defn run [& args]
  (doseq [source args]
    (compile source)))
