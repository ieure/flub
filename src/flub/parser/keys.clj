;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.keys
  "Map of key symbols to hex key codes."
  (:refer-clojure :exclude [key keys])
  (:require [clojure.string :as string]))

(def key-table
  "Numeric values for keys in program records.

   See 9010a Programming Manual, p. 7-5, table 7-2."
  {:0 0x00
   :1 0x01
   :2 0x02
   :3 0x03
   :4 0x04
   :5 0x05
   :6 0x06
   :7 0x07
   :8 0x08
   :9 0x09
   :a 0x0A
   :b 0x0B
   :c 0x0C
   :d 0x0D
   :e 0x0E
   :f 0x0F

   :learn 0x10
   :ram-view 0x11
   :io-view 0x12
   :rom-view 0x13
   :auto-test 0x14
   :bus-test 0x15
   :rom-test 0x16
   :ram-long 0x17
   :ram-short 0x18
   :io-test 0x19
   :prior 0x1a
   :more 0x1b
   :enter-yes 0x1c
   :clear-no 0x1d
   :sts-ctl 0x1e
   :read 0x1f
   :write 0x20
   :ramp 0x21
   :walk 0x22
   :toggl-addr 0x23
   :toggl-data 0x24
   :cont 0x25
   :rpeat 0x26
   :loop 0x27
   :stop 0x28
   :run-uut 0x29
   :progm 0x2a
   :label 0x2b
   :goto 0x2c
   :if 0x2d
   :> 0x2e
   := 0x2f
   :and 0x30
   :or 0x31
   :shift-left 0x32
   :shift-right 0x33
   :incr 0x34
   :decr 0x35
   :compl 0x36
   :exec 0x37
   :reg 0x38
   :read-probe 0x39
   :read-tape 0x3a
   :write-tape 0x3b
   :sync 0x3c
   :setup 0x3d
   :displ 0x3e
   :aux-if 0x3f
   ;; "0x44 is used instad of 0x38 for REG when REG is the first key
   ;; in a program step. For example, REG1 = REG2 would produce 44 01
   ;; 38 02 1C"
   ;; - 9010A Programming Manual p. 7-5
   ;; :reg 0x44 ;; FIXME
   })

(defn normalize [key-ref]
  (cond
   (keyword? key-ref) key-ref
   (or (char? key-ref) (number? key-ref)) (normalize (str key-ref))

   (string? key-ref) (keyword (string/lower-case key-ref))

   (symbol? key-ref) (keyword key-ref)
   true (throw (IllegalArgumentException.
                (format "No such key `%s'" key-ref)))))

(defn key
  ([k] (or (get key-table (normalize k))
           (throw (IllegalArgumentException.
                   (format "No such key `%s'" k))))))

(defn keys [& ks]
  (mapv key ks))
