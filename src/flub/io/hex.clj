;; -*- coding: utf-8 -*-
;;
;; © 2014, 2015 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.io.hex "Parse and generate Fluke format hex."
  (:refer-clojure :exclude [chunk])
  (:use [flub.io.bytes :only [checksum]]
        [clojure.core.match :only [match]])
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [java.text ParseException]
           [java.io IOException StringReader BufferedReader]))

(defn split-str-at "Split string s at position n.
                    Returns [head-str tail-str]"
  [n ^String s] [(subs s 0 n) (subs s n)])

(defn chunk
  "Chunk string into a seq of sz-char-long strings.
   ex: (chunk 2 \"012345\") -> (\"01\" \"23\" \"45\")"
  [sz ^String s]
  (lazy-seq
   (when-not (or (string/blank? s) (< (count s) sz))
     (let [[head tail] (split-str-at 2 s)]
       (cons head (chunk sz tail))))))



(defn split-checksum
  "Return [data-bytes checksum-bytes] for a Fluke hex line."
  [^String hex]
  (if (= "00" hex) ["00" "00"]          ; Checksum of 0 is 0
      ;; Multi-line records use * or $ to separate data from checksum
      (let [[bytes sum] (string/split hex #"[\*\$]")]
        ;; If sum was nil, the cksum is in the last two chars of the string
        (if sum [bytes sum] (split-str-at (- (count hex) 2) hex)))))

(defn hex->bytes
  "Returns byte values for a string of hex.
   Input format is AABBCC, with AA, BB, CC each representing
   one byte."
  [^String hex]
  (reduce (fn [acc bt] (conj acc (Long/parseLong bt 16))) []
          (chunk 2 hex)))

(defn line->bytes
  "Parse a single line of Fluke hex into bytes, verifying the checksum."
  [^String hex]
  (let [[byte-hex sum-hex] (split-checksum (subs hex 1))
        sum (Long/parseLong sum-hex 16)
        bs (hex->bytes byte-hex)
        bs-sum (checksum bs)]
    (when-not (= sum bs-sum)
      (throw (IOException.
              (format "Checksum mismatch, declared sum 0x%02X does not match calculated sum 0x%02X"
                      sum bs-sum))))
    bs))

(defn record->bytes [record]
  (mapcat line->bytes record))

(defn- records->bytes [records]
  (map record->bytes records))

(defn eor?
  "Is this the end of a record?

   This is used to partition the input so multi-line records are
   grouped and an be concatenated after decoding."
  [^String hex]
  (if-not (> (count hex) 4) (gensym)    ; ":00", eos marker
    (let [c (nth hex (- (count hex) 3))]
      (when-not (#{\* \$} c) (gensym)))))

(defn hex-record? "Is this a line of hex?" [^String line] (= \: (first line)))

(defn lines->records "Group lines into records."
  [lines] (partition-by eor? (filter hex-record? lines)))

(defn file->records "Group lines of file into records."
  [file]
  (with-open [r (io/reader file)]
    (doall (lines->records (line-seq r)))))

(defn str->records "Break a string of hex data into records."
  [s]
  (doall (-> (StringReader. s)
             (BufferedReader.)
             (line-seq)
             (lines->records))))

 ;; Hex parsing

(defn file->bytes
  "Parse a file into bytes.
   Returns a seq of seqs, each inner seq containing bytes from one record."
  [file]
  (map record->bytes (file->records file)))

(defn str->bytes
  "Parse a string into bytes.
   Returns a seq of seqs, each inner seq containing bytes from one record."
  [^String s]
  (with-open [r (io/reader (StringReader. s))]
    (doall (map record->bytes (lines->records (line-seq r))))))

 ;; Hex generating

(defn- record->hex-dispatch [[h & _] & _]
  (if (= 0x53 h) :variable :fixed))

(defmulti record->hex record->hex-dispatch)

(defmethod record->hex :fixed [bytes]
  (let [sum (checksum bytes)]
    (->> [":" (mapv #(format "%02X" %) bytes) (format "%02X" sum)]
         (flatten)
         (apply str)
         (vector))))

(defmethod record->hex :variable [bytes & [len]]
  (let [len (or len 36)
        hunks (int (/ (count bytes) len))]
    (for [[n hunk] (->> (partition-all len bytes)
                        (interleave (range))
                        (partition 2))]
      (format ":%s%s%02X" (apply str (map #(format "%02X" %) hunk))
              (if (= n hunks) "$" "*")
              (checksum hunk)))))

(defn bytes->str "Return a string of hex for a sequence of record bytes."
  [byte-records]
  (string/join "\n" (flatten (map record->hex byte-records))))
