;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Ian Eure.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.sig
  (:import [java.nio ByteBuffer]))

(def ^:private ^:const starting-offsets [6 8 11 15 0])

(defn shiftbits [x]
  "Return a hash of `x'.

   This takes a 16-bit input value `x' and produces a 10-bit hash by
   XORing its bits together."
  (apply bit-xor (map #(bit-shift-right x %) (butlast starting-offsets))))

(defn- rotbits* "Return a seq of shifted bytes." [byte]
  ;; A seq of `byte' shifted right 8 times
  (map #(bit-shift-right byte %) (range 8)))

(defn rotbits "Return a seq of shifted bytes for `bytes'" [bytes]
  (mapcat rotbits* bytes))

(defmulti sign "Return the Fluke signature of the input." type)

(defmethod sign (Class/forName "[B") [^bytes bytes]
  (sign (ByteBuffer/wrap bytes)))       ; Wrap byte arrays in ByteBuffer

(defmethod sign clojure.lang.PersistentVector [bytes]
  (->> (map byte bytes)
       (byte-array)
       (ByteBuffer/wrap)
       (sign)))

(defmethod sign ByteBuffer [^ByteBuffer bytes]
  (let [l (.capacity bytes)
        ;; Buffer of checksummed bytes
        ^bytes buf (byte-array 16 (byte 0))]
    (loop [os starting-offsets]
      (when (.hasRemaining bytes)
        ;; This XORs the current byte of the input with the bytes of
        ;; `buf' pointed to by positions 0-3 in `os' The result is
        ;; stored into `buf', in the position indicated by the last
        ;; element of `os'.
        (->> (map #(aget buf %) (butlast os))
             (apply bit-xor (.get bytes))
             (aset-byte buf (last os)))
        ;; Decrement offsets
        ;; The os vector contains a list of pointers which sweep
        ;; downwards through the range of `buf'. Each pointer in `os'
        ;; is decremented after the XOR operation, resetting it back
        ;; to the last element of `buf' when it reaches the lower
        ;; bound.
        (recur (mapv #(mod (- % 1) 16) os))))

    ;; Final checksum calculation.
    ;; For each shifted value of buf:
    ;; 1. Rotate the current signature left one bit
    ;; 2. XOR the current bits of buf with the hashed bits of sig AND 1
    ;; 3. OR those two values together
    ;; 4. Truncate to 16 bits
    ;; 5. Terminate when all `buf' bits have been processed.
    (reduce
     #(mod (bit-or (bit-shift-left %1 1)
                   (bit-and 1 (bit-xor %2 (shiftbits %1)))) 65536)
     0 (rotbits buf))))
