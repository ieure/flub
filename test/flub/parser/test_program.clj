;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.test-program
  (:use [clojure.test]
        [clojure.pprint]
        [the.parsatron :only [run char attempt either always never choice >>]])
  (:require [flub.parser.program :as p]))

(deftest test-program-decl
  (is (= :MAIN (run p/program-decl "PROGRAM MAIN 2 BYTES")))
  (is (= 0 (run p/program-decl "PROGRAM 0 2 BYTES")))
  (is (= :MAIN (run p/program-decl "PROGRAM MAIN")))
  (is (= 0 (run p/program-decl "PROGRAM 0"))))

(deftest test-label
  (is (= [:label :FOO] (run p/label "FOO:")))
  (is (thrown? RuntimeException (run p/label "FOO")))
  (is (thrown? RuntimeException (run p/label ":"))))

(deftest test-statement
  (run p/statement "DPY-#")
  (run p/statement "DPY-SILENCING SOUND")
  (run p/statement "EX DELAY")
  (run p/statement "REGF=6800")
  (run p/statement "QUIET:")
  (run p/statement "WRITE @ REGF=00"))

(deftest test-program
  (is (= [:program :MAIN
          '([:display "#"]
              [:display "SILENCING SOUND"]
                [:execute :DELAY]
                  [:assign [:register 15] [:term 26624]]
                    [:label :QUIET]
                      [:write [:register 15] [:term 0]])]
         (run p/program "PROGRAM MAIN
   DPY-#
   DPY-SILENCING SOUND
   EX DELAY
   REGF=6800
QUIET:
   WRITE @ REGF=00
"))))

(deftest test-program*
  (is (not (empty? (p/string->ast "PROGRAM MAIN
! foo
    ! foo
MAINTEST:
   EX MAINPROCESSOR
   GOTO COMMON

NEXTSW:
   IF REGE AND 2 >0 GOTO SW6JOFF
   GOTO SW6JON

")))))

(deftest test-program-2
    (is (not (empty? (p/string->ast
             "PROGRAM MAIN
   DPY-#
   DPY-SILENCING SOUND
   EX DELAY
   REGF=6800
QUIET:
   WRITE @ REGF=00
   INC REGF
   IF 6820 > REGF GOTO QUIET
   DPY-#
   DPY-BUS TEST
   EX DELAY
   BUS TEST
   DPY-#
   DPY-WHICH PROCESSOR ARE YOU TESTING
   EX DELAY
MAINLOOP:
   DPY-CPU LOCATION 0=4M  1=4J  2=4E \\1
   IF REG1 > 2 GOTO MAINLOOP
   IF REG1 = 0 GOTO MAINTEST
   IF REG1 = 1 GOTO MOTIONTEST
   IF REG1 = 2 GOTO SOUNDTEST

MAINTEST:
   EX MAINPROCESSOR
   GOTO COMMON
MOTIONTEST:
   EX MOTION
   GOTO COMMON
SOUNDTEST:
   EX SOUND
   GOTO COMMON

COMMON:
   DPY-#RAM TESTS
   EX DELAY

   DPY-#TESTING RAM AT 1K
   EX  RAM1KTEST

   DPY-#TESTING RAMS AT 3E AND 3F
   EX   RAM3EFTEST

   DPY-#TESTING RAMS AT 3K AND 3L
   EX   RAM3KLTEST

   DPY-#TESTING RAMS AT 3H AND 3J
   EX   RAM3HJTEST

   DPY-#ALL RAM TESTS COMPLETE
   EX DELAY

   DPY-TESTING DIP SWITCHES
   EX DELAY

   DPY-PRESS CONT TO STEP
   EX DELAY

   REG2=0
   REGF=67FF
SWITCHLOOP:
   INC REGF
   INC REG2
   READ @REGF
   IF REGE AND 1 >0 GOTO SW6KOFF
   GOTO SW6KON
NEXTSW:
   IF REGE AND 2 >0 GOTO SW6JOFF
   GOTO SW6JON
ADDTEST:
   IF REGF >6807 GOTO CONT
   GOTO SWITCHLOOP

SW6KON:
   DPY-#
   DPY-6K NUMBER $2 ON
   STOP
   GOTO NEXTSW

SW6JON:
   IF REGF=6807 GOTO CONT
   DPY-#
   DPY-6J NUMBER $2 ON
   STOP
   GOTO ADDTEST

SW6KOFF:
   DPY-#
   DPY-6K NUMBER $2 OFF
   STOP
   GOTO NEXTSW

SW6JOFF:
   IF REGF=6807 GOTO CONT
   DPY-#
   DPY-6J NUMBER $2 OFF
   STOP
   GOTO ADDTEST

CONT:
   DPY-#
   DPY-DIP TEST COMPLETE
   EX DELAY
   DPY-ATTEMPTING TO RUN SECTION
   RUN UUT
   EX DELAY
   DPY-COMPLETE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !DELAY ROUTINE

")))))

#_(deftest test-parsing-source
    (prn (p/string->ast (slurp (io/resource "program.s")))))
