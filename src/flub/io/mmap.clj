;; -*- coding: utf-8 -*-
;;
;; © 2013 Ian Eure.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.io.mmap
  (:import [java.nio ByteBuffer]
           [java.io FileInputStream]))

(def ^:private READ_ONLY
  (java.nio.channels.FileChannel$MapMode/READ_ONLY))

(defn mmap "Memory-map the file named f.  Returns a ByteBuffer." [^String f]
  (let [channel (.getChannel (FileInputStream. f))]
    (.map channel READ_ONLY 0 (.size channel))))
