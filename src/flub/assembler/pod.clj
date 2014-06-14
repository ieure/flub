;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.assembler.pod
  (:refer-clojure :exclude [resolve])
  (:use [clojure.core.match :only [match]]
        [clojure.math.numeric-tower :only [expt]]
        [flub.assembler.core]
        [flub.io.bytes :only [string->bytes int->lebs]]))

(defn- extract-force "Return a seq of (bit name) for all forcing lines."
  [poddef]
  (->> (filter (fn [[fl? & _]] (= :FORCELN fl?)) poddef)
       (map (fn [[_ name def]] (reverse (cons name (emit def)))))))

(def ^:const force-defaults
  (apply hash-map (interleave (range 8) (repeat "       "))))

(defn- force-pad [name]
  (apply str name (repeat (- 7 (count name)) " ")))

(defn- force-recs
  "Return forcing line records."
  [forcedef]
  (let [mask [0x0D (reduce bit-or (map #(expt 2 (first %)) forcedef))]
        names (->> (flatten (map (fn [[bit name]] [bit (force-pad name)]) forcedef))
                   (apply hash-map)
                   (conj force-defaults))]
    [mask
     (vec (cons 0x0E (string->bytes (apply str (map names (range 4))))))
     (vec (cons 0x0F (string->bytes (apply str (map names (range 4 8))))))]))

(defemit :PODDEF [state [_ & defs]]
  (conj (remove nil?
                (map #(match %
                             [:BUS_TEST_ADDR & _] (emit state %)
                             [:RUN_UUT_ADDR & _]  (emit state %)
                             [:FORCELN & fln]     nil
                             ;; This shouldn't happen.
                             :else %)
                     defs))
        (force-recs (extract-force defs))))
