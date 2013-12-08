;; -*- coding: utf-8 -*-
;;
;; © 2013 Ian Eure.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flubj.io.hex
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron]
        [flubj.parser :only [hex-byte]])
  (:import [the.parsatron ParseError Continue]))

(defn checksum "Compute a simple checksum of bytes."
  [bytes] (-> (reduce + bytes) (unchecked-remainder-int 256)))



(defn checksum-error "Signal a checksum error."
  [expected actual]
  (fn [{:keys [pos] :as state} cok cerr eok eerr]
    (eerr (expect-error (format "checksum 0x%x, got 0x%x" expected actual) pos))))

(def record-bytes (>> (token #{\:}) (many1 hex-byte)))

(def single-line-record
  (let->> [bytes record-bytes
           cksum (either (>> (either (eof) (char \newline)) (always nil))
                         (>> (token #{\$}) hex-byte))]
          (if (and cksum (not= cksum (checksum bytes)))
            (checksum-error (checksum bytes) cksum)
            (always (vec bytes)))))

(defn multi-line-record
  "Consume a multi-line record.

   These records MUST have checksums on every line. A `*' separates
   the data bytes from the checksum bytes for all lines except the
   last, where `$' is the separator."
  ([] (multi-line-record nil))

  ([prev]
     (fn [state cok cerr eok eerr]
       (Continue.
        #((let->> [bytes record-bytes
                   sep (token #{\$ \*})
                   cksum hex-byte
                   _ (either (eof) (char \newline))]
                  (let [real-sum (checksum bytes)]
                    (cond
                     (not= cksum real-sum) (checksum-error real-sum cksum)
                     (= sep \*) (multi-line-record (concat prev bytes))
                     :else (always (vec (concat prev bytes))))))
          state cok cerr eok eerr)))))

(def continuation? "Is the current line one containing a partial record?"
  (let->> [cont (lookahead (>> (char \:)
                               (many1 (token #{\0 \1 \2 \3 \4 \5 \6
                                               \7 \8 \9 \A \B \C \D
                                               \E \F}))
                               (choice (eof)
                                       (char \*)
                                       (any-char))))]
          (always (= \* cont))))

(def multi-or-single-line-record
  (let->> [cont? continuation?]
          (if cont? (multi-line-record)
              single-line-record)))

(def hex-parser
  (many1 multi-or-single-line-record))

 ;; User serviceable parts

(defn parse-file [file]
  (run hex-parser (slurp file)))
