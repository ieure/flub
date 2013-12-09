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

(defmacro enforce [bytes cksum if-ok]
  `(let [real-sum# (checksum ~bytes)]
     (if (not= real-sum# ~cksum)
       (checksum-error real-sum# ~cksum)
       ~if-ok)))

(def record-bytes (>> (token #{\:}) (many1 hex-byte)))

(def single-line-record
  (let->> [bytesum record-bytes
           _ (>> (either (eof) (char \newline)))]
          (let [bytes (butlast bytesum)
                cksum (last bytesum)]
            (enforce bytes cksum (always (vec bytes))))))

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

(def multi? "Is the current line part of a multi-line record?"
  (let->> [cont (lookahead (>> (char \:)
                               (many1 (token #{\0 \1 \2 \3 \4 \5 \6
                                               \7 \8 \9 \A \B \C \D
                                               \E \F}))
                               (choice (eof)
                                       (token #{\* \$})
                                       (any-char))))]
          (always (boolean (#{\* \$} cont)))))

(def multi-or-single-line-record
  (let->> [mult? multi?]
          (if mult? (multi-line-record)
              single-line-record)))

(def hex-parser
  (many1 multi-or-single-line-record))

 ;; User serviceable parts

(defn str->bytes [^String s]
  (run multi-or-single-line-record s))

(defn parse-file [file]
  (run hex-parser (slurp file)))
