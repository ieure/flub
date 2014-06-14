;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.assembler.core
  (:refer-clojure :exclude [resolve])
  (:require [flub.keys :as k]
            [clojure.string :as string]
            [clojure.stacktrace :as stacktrace]
            [taoensso.timbre :as log])
  (:use [flub.io.bytes :only [string->bytes int->lebs]]
        [slingshot.slingshot :only [throw+]]
        [clojure.core.match :only [match]]
        [clojure.walk :only [prewalk]]
        [clojure.tools.macro :only [macrolet]]
        [clojure.pprint]))



(declare emit)

(defn vcc "Concatenates & flattens its arguments."
  [& seqs] (vec (flatten seqs)))

(defn vk "Concatenates & flattens its arguments, treating keywords as keys"
  [& seq]
  (loop [[head & tail :as srest] seq
         acc []]
    (cond
     (nil? srest)    acc
     (nil? head)     (recur tail acc)
     (keyword? head) (recur tail (conj acc (k/key head)))
     (coll? head)    (recur tail (vec (concat acc head)))
     true            (recur tail (conj acc head)))))

(defn execution? "Is this state part of an :EXECUTE?"
  [{:keys [stack]}]
  (contains? stack :EXECUTE))


;; Scanning for & resolving program names & labels
;;
;; Fluke source has two kinds of symbols: Program names and
;; labels. Labels are visible only within the scope of the program
;; containing them, while programs are visible to all other programs
;; in the same source file, or other source files which include them.
;;
;; Program name symbols must be resolved to numeric values, and
;; programs must be emitted in numeric order. Mixing numeric and
;; symbolic program identifiers will probably blow up pretty
;; spectacularly.
;;
;; Symbol tables are vectors of strings; the string is the symbol from
;; the source. Resolving a symbol returns the index of the symbol in
;; its vector. This is horribly inefficient, but given the small
;; tables, it's probably not worth improving at this time.

(defn scan-prognames "Scan for program names in ast."
  [ast]
  (let [defs (transient [])]
    ;; FIXME - What about `PROGRAM 0'?
    (prewalk #(do (match % [:PROGRAM_HEAD [:SYMBOL s]] (conj! defs s)
                           :else nil) %) ast)
    (let [defs (persistent! defs)]
      (log/tracef "Found progs %s" defs)
      defs)))

(defn scan-labels "Scan for labels in ast."
  [ast]
  (let [defs (transient [])]
    (prewalk #(do (match % [:LABEL [:SYMBOL s]] (conj! defs s)
                           :else nil) %) ast)
    (persistent! defs)))

;; Resolving symbols

(defn resolve "Resolve symbol s in symtable tbl."
  [tbl s]
  (let [n (.indexOf (or tbl []) s)]
    (if (< n 0)
      (throw+ {:undefined :symbol
               :symbol s
               :symtab tbl})
      n)))

(defn resolve-prog "Resolve a program reference."
  [{:keys [progs] :as state} prog]
  (let [out (match prog
                   [:SYMBOL s] (resolve progs s)
                   :else (emit state prog))]
    (log/tracef "@%s resolved prog `%s' -> `%s'"
                (string/join "->" (:stack state)) prog out)
    out))

(defn resolve-label "Resolve a label."
  [{:keys [labels] :as state} label]
  (match label
         [:SYMBOL s] (resolve labels s)
         :else (emit state label)))

(defn- make-label-table
  "Make a table of labels and indexes into the program bytes.

   Offsets point at the next step AFTER the label, relative to the
   start of the program, including the 0x53 SOP marker.
   See 9010A Programming Manual p. 7-6."
  ([bytes] (make-label-table [] 1 bytes))
  ([table i bytes]
     (match [bytes]
            [(a :guard empty?)] (flatten table)
            [([43 n & rest] :seq)] (recur (conj table
                                                (cons n (int->lebs (+ 1 i))))
                                          (+ i 2) rest)
            :else (recur table (+ i 1) (rest bytes)))))

 ;; Handling numbers

(defn numeric [[type ^String val]]
    (Integer/parseInt val (condp = type :DEC 10 :HEX 16)))


;; Emitting bytes
;;
;; The byte emitter is built with multimethods which dispatch to the
;; correct emitter for that part of the AST. The emit multifn takes
;; two arguments, `state' and `ast'. State contains a map containing
;; program symbols and label symbols for the current program. The
;; :PROGRAM and :S methods push new info into the state, and most
;; other methods don't care.
;;
;; defemit is used to create a new emitter, and copes with error
;; handling and tracing.

(defn emit-dispatch
  "Dispatch an emit call.

   With one arg, dispatch to :stateless.
   Otherwise, dispatch on the first symbol of the 1th arg."
  ([arg] :stateless)
  ([state [node & _] & _] node))

(def ^:constant no-state "Empty (default) state."
  {:stack [] :labels [] :progs []})

(defmulti emit "Emit bytes for the AST" emit-dispatch)

;; For cases where no state is passed in
(defmethod emit :stateless [ast]
  (emit no-state ast))

(defmacro defemit "Define an AST emitter."
  [kw args & body]
  `(defmethod emit ~kw [state# args#]
     (let [state# (update-in state# [:stack] conj ~kw)
           stack# (:stack state#)]
       (log/tracef "@%s => %s" (string/join "->" stack#) args#)
       (let [out# ((fn ~args ~@body) state# args#)]
         (log/tracef "%s <- %s" out# (string/join "->" stack#))
         out#))))

;; Fallback emitter - this will break things pretty badly.
(defmethod emit :default [{:keys [stack] :as state} [s & _ :as subtree]]
  #_(throw+ {:unhandled s
           :subtree subtree
           :stack stack})
  ;; FIXME for now. Once things are in a better state, it should throw
  ;; an exception.
  [:FIXME {:stack stack
           :terminal s
           :subtree subtree}])

;; The start of the source scans for progam symbols and pushes them
;; into the state
(defemit :S [state [s & rest]]
  (let [state (assoc state :progs (scan-prognames rest))]
    (log/tracef "@%s - State now: %s" (string/join "->" (:stack state)) state)
    (mapcat (partial emit state) rest)))

(defemit :INCLUDED [state [inc rest]]
  (emit state rest))

(defemit :REGISTER [state [r reg-or-sym]]
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

(defemit :AUX [state [d dstr]]
  (vcc (k/key :aux-if)
       (emit state dstr)))

(defemit :STRING [state [s sval]]
  ;; FIXME - need to replace symbol names
  (vcc (string->bytes sval) 0x74))

(defemit :GOTO [state [g label]]
  (vcc (k/key 'goto)
       (resolve-label state label)))

;; The start of a program scans for progam labels and pushes them into
;; the state
(defemit :PROGRAM [state [p & rest]]
  (let [state (assoc state :labels (scan-labels rest))]
    (log/tracef "@%s - State now: %s" (string/join "->" (:stack state)) state)
    (map (partial emit state) rest)))

;; Containers - these just emit their contents

(defemit :PROGRAM_HEAD [state [ph prog]]
  (vcc 0x1a (resolve-prog state prog)))

(defemit :PROGRAM_BODY [state [p & rest]]
  ;; "0x53 indicates the start-of-program, and is always the first
  ;; byte in a program."
  ;; "0x50 indicates the end-of-program, and is always the last byte
  ;; of a program (labels may follow)."
  ;;
  ;; - 9010A Programming Manual p. 7-5
  (let [prog-bytes (vcc [0x53]
                        (map (partial emit state) rest)
                        [0x50])]
        (vcc prog-bytes
             (make-label-table prog-bytes))))

(defemit :STATEMENT [state [s & rest]]
  (mapv (partial emit state) rest))

(defemit :TERM [state [p & rest]]
  (map (partial emit state) rest))

(defemit :TERM_UNOP [state [tu cmd n]]
  (let [n (if n (numeric n) 1)]
    (repeat n (k/key
               (condp = cmd
                 "CPL" :compl
                 "DEC" :decr
                 "INC" :incr
                 "SHR" :shift-right
                 "SHL" :shift-left)))))

(defemit :EXPR [state [p & rest]]
  (map (partial emit state) rest))

(defemit :ADDR [state [a rest]]
  (vec (flatten (emit state rest))))

(defemit :AND [state & _]
  (k/key :and))

(defemit :OR [state & _]
  (k/key :or))

(defemit :ADDRESS_BLOCK [state [ab & range]]
  (vcc (map #(emit state %) range)))

(defemit :LABEL [state [l label]]
  (vcc (k/key :label)
       (resolve-label state label)))

(defemit :EXECUTE [state [e prog]]
  (vk :exec (emit state prog)))

(defemit :SYMBOL [{:keys [labels progs]} [_ sym]]
  (if execution?
    (resolve progs sym)
    (resolve-label labels sym)))

(defemit :WRITE [state [w target _ val]]
  (vcc (k/key :write)
       (emit state target)
       (k/keys :enter-yes :=)
       (emit state val)
       (k/key :enter-yes)))

(defemit :IF [state [i expr-a cond expr-b goto]]
  (vcc (k/key :if)
       (emit state expr-a)
       (map k/key cond)
       (emit state expr-b)
       (emit state goto)))

(defemit :BUS_TEST [state & _]
  (k/keys :bus-test :enter-yes))

(defemit :RAM_TEST [state [rtest type ablock]]
  (vcc (k/keys (condp = type
                 "SHORT" :ram-short
                 "LONG" :ram-long))
       (emit state ablock)
       (k/key :enter-yes)))

;; Not sure if this is correct.
(defemit :POD [state [p pod]]
  [])

(defemit :SYNC [state [s type]]
  (k/keys :sync
          ({"ADDRESS" :a
            "DATA" :d
            "FREE-RUN" :f} type)
          :enter-yes))

(defemit :STOP [state & _]
  (k/key :stop))

(defemit :RUN_UUT [state [ru & [_ addr]]]
  (vcc (k/key :run-uut)
       (if addr (emit state addr)
           [])
       (k/key :enter-yes)))

(defemit :REG_ASSIGN [state [_ reg val]]
  (vcc (emit state reg)
       (emit state val)
       (k/key :enter-yes)))

(defemit :DTOG [state [dtog addr expr bit bitexpr]]
  (vk :toggl-data
      (emit state addr)
      :enter-yes
      (emit state expr)
      :enter-yes
      (emit state bitexpr) ;; FIXME handle "*"
      :enter-yes))

(defemit :READ [state [read addr]]
  (vk :read
      (emit state addr)
      :enter-yes))

(defemit :READ_PROBE [state & _]
  (vk :read-probe :enter-yes))

(defemit :STS [state & _]
  (vk :sts-ctl))

(defemit :CTL [state & _]
  (vk :sts-ctl))

(defemit :WALK [state [walk addr _ val]]
  (vk :walk
      (emit state addr)
      :enter-yes
      (emit state val)
      :enter-yes))

(defemit :ATOG [state [atog loc _ bit]]
  (vk :toggl-addr
      (emit state loc)
      :enter-yes))

(defemit :ROM_TEST [state [romtest addrs _ sig]]
  (vk :rom-test
      (when addrs (emit state addrs))
      :enter-yes
      (when sig (emit state sig))
      :enter-yes))

(defemit :RAMP [state [ramp addr]]
  (vk :ramp (emit state addr) :enter-yes))

 ;; Setup

#_(defn- merge-traps [acc branch]
  (match branch
         [:TRAP n] (bit-or acc n)
         :else n))

#_(defemit :SETUP [state [setup & rest]]
  (let [partial (emit state rest)
        traps (reduce merge-traps partial 0)
        ]

    )
  )

(def ^:constant setup-trap-masks
  {:SETUP_BAD_POWER_SUPPLY  2r10000000
   :SETUP_ILLEGAL_ADDRESS   2r00100000
   :SETUP_ACTIVE_INTERRUPT  2r00100000
   :SETUP_ACTIVE_FORCE_LINE 2r00001000
   :SETUP_CONTROL_ERROR     2r00000100
   :SETUP_ADDRESS_ERROR     2r00000001
   :SETUP_DATA_ERROR        2r00000010})

(defemit :SETUP_TRAP [state [trap [line] yn]]
  (let [val (emit state yn)]
    [:TRAP (if (= val 0) 0
               (get setup-trap-masks line))]))

(defemit :BUS_TEST_ADDR [state [_ addr]]
  (vcc 0x05 (emit state addr)))

(defemit :RUN_UUT_ADDR [state [_ addr]]
  (vcc 0x06 (emit state addr)))

 ;; User-servicable parts

(defn ast->bytes "Emit bytes for an AST."
  [ast] (emit ast))
