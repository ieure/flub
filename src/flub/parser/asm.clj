;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.asm
  (:refer-clojure :exclude [resolve])
  (:require [flub.parser.keys :as k]
            [clojure.string :as string])
  (:use [flub.io.bytes :only [string->bytes]]
        [slingshot.slingshot :only [throw+]]
        [clojure.core.match :only [match]]
        [clojure.walk :only [prewalk]]
        [clojure.tools.macro :only [macrolet]]))



(declare emit)

(defn vcc "Concatenates & flattens its arguments."
  [& seqs] (vec (flatten seqs)))


;; Scanning for & resolving program names & labels
;;
;; Fluke source has two kinds of symbols: Program names and
;; labels. Labels are visible only within the scope of the program
;; containing them, while programs are visible to all other programs
;; in the same source file.
;;
;; Symbols must be resolved to numeric values, and programs must be
;; emitted in numeric order. Mixing numeric and symbolic program
;; identifiers will probably blow up pretty spectacularly.
;;
;; Symbol tables are vectors of strings; the string is the symbol from
;; the source. Resolving a symbol returns the index of the symbol in
;; its vector. This is horribly inefficient, but given the small
;; tables, it's probably not worth improving at this time.

(defn scan-prognames "Scan for program names in ast."
  [ast]
  (let [defs (transient []), uses (transient [])]
    (prewalk #(do (match % [:PROGRAM_HEAD [:SYMBOL s]] (conj! defs s)
                           [:EXECUTE [:SYMBOL s]]      (conj! uses s)
                           :else nil) %) ast)
    {:defs (persistent! defs)
     :uses (persistent! uses)}))

(defn scan-labels "Scan for labels in ast."
  [ast]
  (let [defs (transient []), uses (transient [])]
    (prewalk #(do (match % [:LABEL [:SYMBOL s]] (conj! defs s)
                           [:GOTO [:SYMBOL s]]  (conj! uses s)
                           :else nil) %) ast)
    {:defs (persistent! defs)
     :uses (persistent! uses)}))

;; Resolving symbols

(defn resolve "Resolve symbol s in symtable tbl."
  [tbl s]
  (let [n (.indexOf (or tbl []) s)]
    (if (< n 0)
      (throw (NoSuchFieldException.
              (format "Symbol `%s' does not exist in table: %s" s tbl)))
      n)))

(defn resolve-prog "Resolve a program reference."
  [{:keys [progs] :as state} prog]
  (match prog
         [:SYMBOL s] (resolve progs s)
         :else (emit state prog)))

(defn resolve-label "Resolve a label."
  [{:keys [labels] :as state} label]
  (match label
         [:SYMBOL s] (resolve labels s)
         :else (emit state label)))


;; Emitting bytes
;;
;; The byte emitter is built with multimethods which dispatch to the
;; correct emitter for that part of the AST. The emit multifn takes
;; two arguments, `state' and `ast'. State contains a map containing
;; program symbols and label symbols for the current program. The
;; :PROGRAM and :S methods push new info into the state, and most
;; other methods don't care. The defemit and with-thunk macros handle
;; passing state between emitters. If an emitter needs to call
;; another, calling `(thunk ast)' will automatically inject the
;; current state into the call. Emitters which need to alter state
;; must do so manually.

(defn emit-dispatch
  "Dispatch an emit call.

   If there is only one arg, assume there is no state and dispatch to
   :stateless. This lets us do arity overloading in the multimethod."
  [& args]
  (cond
   (= 1 (count args)) :stateless
   true               (ffirst (next args))))

(def ^:constant no-state {:stack [] :labels [] :progs []})

(defmulti emit "Emit bytes for the AST" emit-dispatch)

;; For cases where no state is passed in
(defmethod emit :stateless [ast]
  (emit no-state ast))

(defmacro defemit "Define an AST emitter."
  [kw args & body]
  `(defmethod emit ~kw ~args
     (let [~'state (update-in ~'state [:stack] conj ~kw)
           stack# (:stack ~'state)]
       (try
         ~@body
         (catch Exception e#
           (throw+ {:stack stack#
                    :exception e#}))))))

;; Fallback emitter - this will break things pretty badly.
(defemit :default [{:keys [stack] :as state} [s & _]]
  ;; FIXME for now. Once things are in a better state, it should throw
  ;; an exception.
  [:FIXME {:stack stack
           :terminal s}])

(defemit :REGISTER [state [_ reg-or-sym]]
  (k/keys :reg reg-or-sym))

(defemit :HEX [state [h vals]]
  (mapv k/key vals))

(defemit :DEC [state [d vals]]
  (mapv k/key vals))

(def ^:constant unop-map
  {"CPL" 'compl
   "DEC" 'decr
   "INC" 'incr
   "SHL" 'shift-left
   "SHR" 'shift-right})

(defemit :UNARY [state [_ op target]]
  (vcc (k/keys (get unop-map op))
       (emit state target)))

(defemit :DISPLAY [state [d dstr]]
  (vcc (k/key 'displ)
       (emit state dstr)))

(defemit :STRING [state [s sval]]
  ;; FIXME - need to replace symbol names
  (string->bytes sval))

(defemit :GOTO [state [g label]]
  (vcc (k/key 'goto)
       (resolve-label state label)))

;; The start of the source scans for progam symbols and pushes them
;; into the state
(defemit :S [state [s & rest]]
  (mapcat (partial emit (assoc state :progs (:defs (scan-prognames rest))))
       rest))

;; The start of a program scans for progam labels and pushes them into
;; the state
(defemit :PROGRAM [state [p & rest]]
  (map (partial emit (assoc state :labels (:defs (scan-labels rest)))) rest))

;; Containers - these just emit their contents

(defemit :STATEMENT [state [s & rest]]
  (mapv (partial emit state) rest))

(defemit :TERM [state [p & rest]]
  (map (partial emit state) rest))

(defemit :EXPR [state [p & rest]]
  (map (partial emit state) rest))

(defemit :ADDR [state [a rest]]
  (vec (flatten (emit state rest))))

(defemit :PROGRAM_HEAD [state [ph prog]]
  (vcc 0x1a (resolve-prog state prog)))

(defemit :PROGRAM_BODY [state [p & rest]]
  ;; "0x53 indicates the start-of-program, and is always the first
  ;; byte in a program."
  ;; "0x50 indicates the end-of-program, and is always the last byte
  ;; of a program (labels may follow)."
  ;;
  ;; - 9010A Programming Manual p. 7-5
  (vcc [0x53]
       (map (partial emit state) rest)
       [0x50]))

(defemit :LABEL [state [l label]]
  (vcc (k/key :label)
       (resolve-label state label)))

(defemit :EXECUTE [state [e prog]]
  (vcc (k/key :exec)
       (resolve-prog state prog)))

(defemit :WRITE [state [w target _ val]]
  (vcc (k/key :exec)
       (emit state target)
       (k/key :=)
       (emit state val)))

(defemit :IF [state [i expr-a cond expr-b goto]]
  (vcc (k/key :if)
       (emit state expr-a)
       (map k/key cond)
       (emit state expr-b)
       (emit state goto))
  )

 ;; User-servicable parts

(defn ast->bytes "Emit bytes for an AST."
  [ast] (emit ast))
