;; -*- coding: utf-8 -*-
;;
;; © 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.assembler.setup
  (:use [flub.assembler.core]))


#_(defn- merge-traps [acc branch]
    (match branch
           [:TRAP n] (bit-or acc n)
           :else n))

#_(defemit :SETUP [state [setup & rest]]
    (let [partial (emit state rest)
          traps (reduce merge-traps partial 0)
          ]

      )
    )

(def ^:constant setup-trap-masks
  {:SETUP_BAD_POWER_SUPPLY  2r10000000
   :SETUP_ILLEGAL_ADDRESS   2r00100000
   :SETUP_ACTIVE_INTERRUPT  2r00100000
   :SETUP_ACTIVE_FORCE_LINE 2r00001000
   :SETUP_CONTROL_ERROR     2r00000100
   :SETUP_ADDRESS_ERROR     2r00000001
   :SETUP_DATA_ERROR        2r00000010})

(defemit :SETUP_TRAP [state [trap [line] yn]]
  (let [val (emit state yn)]
    [:TRAP (if (= val 0) 0
               (get setup-trap-masks line))]))

(defemit :BUS_TEST_ADDR [state [_ addr]]
  (vcc 0x05 (emit state addr)))

(defemit :RUN_UUT_ADDR [state [_ addr]]
  (vcc 0x06 (emit state addr)))
