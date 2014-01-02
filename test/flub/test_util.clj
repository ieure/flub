;; -*- coding: utf-8 -*-
;;
;; © 2013 Ian Eure
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.test-util
  (:use [clojure.test])
  (:require [instaparse.core :as insta]
            [instaparse.failure :as fail]))

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
