;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.cond-test
  (:use [clojure.test]
        [the.parsatron :only [run char >>]])
  (:require [flub.parser.cond :as c]))

(deftest test-goto
  (is (= [:goto 3] (run c/goto "GOTO 3")))
  (is (= [:goto :FOO] (run c/goto "GOTO FOO"))))

(deftest test-if
  (is (= [:if [:> [:term [:register 1]] [:term 7]] [:goto 0]]
         (run c/ifcond "IF REG1 > 7 GOTO 0")))
  (is (= [:if [:= [:term [:register 1]] [:term 0]] [:goto 1]]
         (run c/ifcond "IF REG1 = 0 GOTO 1")))
  (is (= [:if [:= [:term [:register 1]] [:term 1]] [:goto 2]]
         (run c/ifcond "IF REG1 = 1 GOTO 2")))
  (is (= [:if [:= [:term [:register 1]] [:term 2]] [:goto 3]]
         (run c/ifcond "IF REG1 = 2 GOTO 3")))
  (is (= [:if [:= [:term [:register 1]] [:term 3]] [:goto 4]]
         (run c/ifcond "IF REG1 = 3 GOTO 4")))
  (is (= [:if [:= [:term [:register 1]] [:term 4]] [:goto 5]]
         (run c/ifcond "IF REG1 = 4 GOTO 5")))
  (is (= [:if [:= [:term [:register 1]] [:term 5]] [:goto 6]]
         (run c/ifcond "IF REG1 = 5 GOTO 6")))
  (is (= [:if [:= [:term [:register 1]] [:term 6]] [:goto 7]]
         (run c/ifcond "IF REG1 = 6 GOTO 7")))
  (is (= [:if [:= [:term [:register 1]] [:term 7]] [:goto 8]]
         (run c/ifcond "IF REG1 = 7 GOTO 8")))
  (is (= [:if [:= [:term [:register 14]] [:term 0]] [:goto 1]]
         (run c/ifcond "IF REGE = 0 GOTO 1")))
  (is (= [:if [:= [:expr [:and [:term [:register 14]] [:term 1]]] [:term 1]] [:goto 1]]
         (run c/ifcond "IF REGE AND 1 = 1 GOTO 1")))
  (is (= [:if [:> [:expr [:and [:term [:register 14]] [:term 128]]] [:term 0]] [:goto 3]]
         (run c/ifcond "IF REGE AND 80 > 0 GOTO 3")))
  (is (= [:if [:= [:expr [:and [:term [:register 0xE]] [:term [:register 0xB]]]]
               [:term [:register 0xB]]]
          [:goto 0xF]]
         (run c/ifcond "IF REGE AND REGB = REGB GOTO F"))))
