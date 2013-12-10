;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.io.bytes
  (:use [clojure.string :only [join]]))

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

(defn bytes->pretty
  ([bytes] (bytes->pretty bytes default-split))
  ([bytes split]
     (->>
      (partition-all split bytes)
      (interleave (offsets split (count bytes)))
      (partition 2)
      (map bytes->pretty*)
      (join "\n" ))
     ))

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

 ;; Strings

(defn- byte->7bit [byte]
  (bit-and 2r01111111 byte))

(defn ^String bytes->string [bytes]
  (->> (map byte->7bit bytes)
       (take-while #(> % 0))
       (map clojure.core/char)
       (apply str)))