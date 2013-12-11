;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.sig)

(def ^:private ^:const starting-offsets [6 8 11 15 0])

(defn shiftbits [x]
  "Return a hash of `x'.

   This takes a 16-bit input value `x' and produces a 10-bit hash by
   XORing its bits together."
  (apply bit-xor (map #(bit-shift-right x %) (butlast starting-offsets))))

(defn- rotbits* [byte]
  (map #(bit-shift-right byte %) (range 8)))

(defn rotbits [bytes]
  (mapcat rotbits* bytes))

(defn- signature-1 [[byte & bytes] buf offsets]
  (if byte
    (recur bytes
           (assoc buf (last offsets)
                  (apply bit-xor byte (map #(nth buf %) (butlast offsets))))
           (mapv #(mod (- % 1) 16) offsets))
    (reduce
     #(mod (bit-or (bit-shift-left %1 1)
                   (bit-and 1 (bit-xor %2 (shiftbits %1)))) 65536)
     0 (rotbits buf))))


(defn signature [bytes]
  (let [buf (vec (repeat 16 0))
        offsets starting-offsets]
    (signature-1 bytes buf offsets)))
