;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.io.record
  (:use [flub.io.bytes :only [merge-bytes bytes->string bit-lookup]]
        [clojure.math.numeric-tower :only [expt]])
  (:require [flub.keys :as k]))

;; 9010a Programming Manual, p7-3, section 7-5.
(def ^:const trap-syms
  {2r00000001 :trap-data-err
   2r00000010 :trap-addr-err
   2r00000100 :trap-ctl-err
   2r00001000 :trap-active-force-line
   2r00010000 :trap-active-interrupt
   2r00100000 :trap-illegal-address
   2r10000000 :trap-bad-power-supply})

(defn traps "Create a map of trap states for a mask."
  [bits] (bit-lookup bits trap-syms))

(defn byte->bool [byte]
  (= byte 0x01))

;; 9010a Programming Manual, p7-2, table 7-1.
(def ^:const bytes->tree-table "Table of bytes -> dispatch values"
  {0x01 :trap
   0x02 :forcing-lines
   0x03 :beep-on-error
   0x04 :exercise-errors
   0x05 :bus-test-addr
   0x06 :run-uut-addr
   0x07 :stall
   0x08 :unstall
   0x09 :line-size
   0x0A :timeout
   0x0B :newline
   0x0C :pod
   0x0D :forcing-lines-available
   0x0E :forcing-line-names-lsb
   0x0F :forcing-line-names-msb
   0x10 :reserved
   0x11 :reserved
   0x12 :reserved
   0x13 :reserved
   0x14 :reserved
   0x15 :reserved
   0x16 :reserved
   0x17 :reserved
   0x19 :address-descriptor
   0x1A :prognum
   0x53 :program-body
   0x00 :eosc})

(defn- bytes->tree-dispatch "Dispatch a call to bytes->tree."
  [[first-byte & _]]
  (get bytes->tree-table first-byte))

(defmulti bytes->tree "Return a semi-AST from a seq of record bytes seqs."
  bytes->tree-dispatch :default :fixme)

;; Fallback for unimplemented instructions
(defmethod bytes->tree :fixme [args]
  [:FIXME {:dispatch (bytes->tree-dispatch args)
           :bytes args}])

(defmethod bytes->tree :trap [[trap mask]]
  [:trap (traps mask)])

(defmethod bytes->tree :forcing-lines [[forcing-lines mask]]
  [:forcing-lines mask])

(defmethod bytes->tree :beep-on-error [[beep-on-error enable]]
  [:beep-on-error (byte->bool enable)])

(defmethod bytes->tree :exercise-errors [[exercise-errors enable]]
  [:exercise-errors (byte->bool enable)])

(defmethod bytes->tree :bus-test-addr [[bus-test-addr & bytes]]
  [:bus-test-addr (merge-bytes bytes)])

(defmethod bytes->tree :run-uut-addr [[run-uut-addr & bytes]]
  [:run-uut-addr (merge-bytes bytes)])

(defmethod bytes->tree :stall [[stall & bytes]]
  [:stall-addr (merge-bytes bytes)])

(defmethod bytes->tree :unstall [[unstall & bytes]]
  [:unstall (merge-bytes bytes)])

(defmethod bytes->tree :line-size [[line-size & bytes]]
  [:line-size (merge-bytes bytes)])

(defmethod bytes->tree :timeout [[timeout & bytes]]
  [:timeout (merge-bytes bytes)])

(defmethod bytes->tree :newline [[newline & bytes]]
  [:newline (merge-bytes bytes)])

(defmethod bytes->tree :pod [[pod & chars]]
  [:pod (bytes->string chars)])

(defmethod bytes->tree :forcing-lines-available [[forcing-lines-available mask]]
  [:forcing-lines-available mask])

(defmethod bytes->tree :forcing-line-names-lsb [[forcing-line-names-lsb & names]]
  [:forcing-line-names-lsb (mapv bytes->string (partition 7 names))])

(defmethod bytes->tree :forcing-line-names-msb [[forcing-line-names-msb & names]]
  [:forcing-line-names-msb (mapv bytes->string (partition 7 names))])

(defmethod bytes->tree :reserved [& _]
  ;;ERROR
  )

(defmethod bytes->tree :address-descriptor [[address-descriptor
                                      la lb lc ld   ; Low address
                                      ha hb hc hd   ; High address
                                      type          ; Address type
                                      _ _ _ _ _     ; Padding
                                      sa sb sc cd]] ; Signature or mask
  (condp = type
    0x01 [:io (merge-bytes [la lb lc ld]) (merge-bytes [ha hb hc hd])
          (merge-bytes [sa sb sc cd])]
    0x02 [:ram (merge-bytes [la lb lc ld]) (merge-bytes [ha hb hc hd])]
    0x03 [:rom (merge-bytes [la lb lc ld]) (merge-bytes [ha hb hc hd])
          (merge-bytes [sa sb sc cd])]))

(defmethod bytes->tree :prognum [[prognum n]]
  [:program n])

(defmethod bytes->tree :program-body [[program-body & body]]
  ;; Label lookup table follows the 0x50 — We don't care, ignore it.
  [:program-body (loop [acc []
                        [byte & bytes] (take-while #(not (#{0x50} %)) body)]
                   (cond
                    (nil? byte) acc     ; End of processing

                    ;; String or aux
                    (#{(k/key :displ) (k/key :aux-if)} byte)
                    (let [[sb bb] (split-with #(not (#{0x74} %)) bytes)]
                      (recur (conj acc (k/key-for byte) (bytes->string sb))
                             (rest bb)))

                    ;; Plain key seq
                    true (recur (conj acc (k/key-for byte)) bytes)))])

;; End of source code
(defmethod bytes->tree :eosc [[eosc]] nil)

(def ^:const forcing-keys #{:forcing-lines
                            :forcing-lines-available
                            :forcing-line-names-lsb
                            :forcing-line-names-msb})

(defn pp "Postprocess parsed records to create forcing line info"
  [recs]
  (let [fls (into {} (filter #(forcing-keys (first %)) recs))
        nameseq (concat (reverse (get fls :forcing-line-names-msb))
                        (reverse (get fls :forcing-line-names-lsb)))
        names (->> (reverse nameseq)
                   ;; Produce a bit mask for each elt
                   (zipmap (map #(expt 2 %) (range)))
                   (remove (fn [[k v]] (= v "")))
                   (map (fn [[k v]] [k (keyword v)]))
                   (into {}))]
    [[:forcing-line-available (bit-lookup (:forcing-lines-available fls) names)]
     [:forcing-lines (bit-lookup (:forcing-lines fls) names)]]))

(defn disass [bytes]
  ;; (map bytes->tree bytes)
  (let [res (map bytes->tree bytes)
        forcing (pp res)]
    (concat forcing res)))
