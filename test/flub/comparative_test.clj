;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.comparative-test
  "Test the Flub parser against 9LC.EXE"
  (:use [clojure.test]
        [flub.test-util])
  (:require [clojure.java.io :as io]
            [flub.assembler.core :as asm]))

(deftest comparative-test
  #_(testing "DELAY.S"
    (let [gold (slurp (io/resource "DELAY.H"))
          lead (asm/source->str (slurp (io/resource "DELAY.S")))]
      (is (= gold lead))))

  (testing "DEMO.S"

    (let [gold (slurp (io/resource "DEMO.H"))
          lead (asm/source->str (slurp (io/resource "DEMO.S")))]
      (is (compiled-to gold lead)))))
