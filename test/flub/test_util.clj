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
