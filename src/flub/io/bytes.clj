;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.io.bytes "Byte-level utils: conversion, checksums, printing"
    (:use [clojure.string :only [join]]
          [clojure.math.numeric-tower :only [expt]]))

(def ^:const pretty-format "%02X")
(def ^:const default-split 16)

(defn bytes->pretty* [[offset bytes]]
  (let [n (count bytes)
        base (repeat n pretty-format)
        formats (if (> n (/ default-split 2))
                  (let [[head tail] (split-at (/ default-split 2) base)]
                    (concat head ["-"] tail))
                  base)]
    (apply format (str "%04X  " (join " " formats)) offset bytes)))

(defn offsets [split len]
  (range 0 len split))

(defn bytes->pretty "Pretty-print bytes"
  ([bytes] (bytes->pretty bytes default-split))
  ([bytes split]
     (->>
      (partition-all split bytes)
      (interleave (offsets split (count bytes)))
      (partition 2)
      (map bytes->pretty*)
      (join "\n"))))

(defn- merge-bytes* "Merge a series of 8-bit values" [bytes]
  (reduce
   (fn [acc [byte shift]]
     (bit-xor acc (bit-shift-left (bit-and byte 0xFF) shift)))
   0
   (partition 2 (interleave bytes
                            (range (* (- (count bytes) 1) 8) -8 -8)))))

(defn swap-bytes "Reverse byte order"
  [bytes]
  (flatten (map reverse (partition 2 bytes))))

(defn merge-bytes
  ([bytes] (merge-bytes bytes :little-endian))
  ([bytes endian]
     (merge-bytes* (if (= :little-endian endian)
                     (swap-bytes bytes)
                     bytes))))

(defn split-bytes "Turn an n-bit Long into a series of 8-bit Ints"
  [^Long value] ;; FIXME - implement this
  )

(defn checksum "Compute a simple checksum of bytes."
  [bytes] (-> (reduce + bytes) (unchecked-remainder-int 256)))

 ;; Strings

(defn- byte->7bit "Strip the high bit from a byte"
  [byte] (bit-and 2r01111111 byte))

(defn ^String bytes->string "Turn a Fluke string into a native string"
  [bytes] (->> (map byte->7bit bytes)
               (take-while #(> % 0))
               (map clojure.core/char)
               (apply str)))

(defn string->bytes
  "Turn a native string into a Fluke string

  \"The ASCII characters in Displkay and AUX I/F steps have the usual
  ASCII codes, except that the eighth but it always a one. For
  example, the ASCII code for \"A\" is 41, so in a Display or Aux I/F
  step it would appear as C1\"
  - Fluke 9010A Programming Manual p. 7-6"
  [^String s]

  (conj (mapv #(bit-or 2r10000000 (int %)) s) 0))

 ;; Bitmask lookup

(defn bit-lookup
  "Given a mask and map/vector of symbols, return a map of symbol enable states."
  [mask syms]
  (let [ss (if (vector? syms) (zipmap (map #(expt 2 %) (range)) syms)
               syms)]
    (into {} (map (fn [[smask kw]]
                    [kw (= smask (bit-and smask mask))]) ss))))
