;; -*- coding: utf-8 -*-
;;
;; © 2013, 2014 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.test-util
  (:use [clojure.test]
        [flub.io.record :only [disass]])
  (:require [clojure.data :as data]
            [instaparse.core :as insta]
            [instaparse.failure :as fail]
            [flub.io.hex :as hex]))

(defmethod assert-expr 'parsed? [msg form]
  `(let [res# ~(first (rest form))
         failed?# (insta/failure? res#)]
     (if failed?#
       (do-report {:type :fail
                   :message ~msg
                   :actual (insta/get-failure res#)})
       (do-report {:type :pass :message ~msg}))
     (not failed?#)))

(defmethod assert-expr 'parsed-to [msg [_ expected form]]
  `(let [res# ~form
         failed?# (insta/failure? res#)]
     (cond
      failed?# (do-report {:type :fail
                           :message ~msg
                           :actual (insta/get-failure res#)})

      (not= ~expected res#) (do-report {:type :fail
                                        :message ~msg
                                        :expected ~expected
                                        :actual res#})

      true (do-report {:type :pass :message ~msg}))))

(defmethod assert-expr 'compiled-to [msg [_ expected form]]
  `(let [res# ~form
         resrecs# (disass (hex/str->bytes res#))
         exprecs# (disass (hex/str->bytes ~expected))]
     (if (not= resrecs# exprecs#)
       (do-report {:type :fail
                   :message ~msg
                   :expected exprecs#
                   :actual resrecs#})
       (do-report {:type :pass :message ~msg}))))
