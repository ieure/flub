;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flubj.io.bytes)

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
