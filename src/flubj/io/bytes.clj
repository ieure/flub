;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flubj.io.bytes)

(def ^:const pretty-format "%02X")

(defn bytes->pretty* [bytes]
  (let [n (count bytes)
        base (repeat n pretty-format)
        formats (if (> n 8)
                  (let [[head tail] (split-at 8 base)]
                    (concat head ["-"] tail))
                  base)]
    (apply format (join " " formats) bytes)))

(defn bytes->pretty [bytes]
  (->> (partition-all 16 bytes)
       (map bytes->pretty*)
       (join "\n" )))

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
