;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser-test
  (:use [clojure.test]
        [the.parsatron :only [run digit char >>]])
  (:require [flub.parser :as p]
            [clojure.java.io :as io]))

(deftest test-term-unary-operator
  (is (= [:inc 3] (run p/term-unary-operator "INC3")))
  (is (= [:inc 7] (run p/term-unary-operator "INC 7")))
  (is (= [:inc 1] (run p/term-unary-operator "INC"))))

(deftest test-term
  (is (= '[:term [:register 10] ([:shr 1] [:inc 1])]
         (run p/term "REGA SHR INC")))
  (is (= '[:term [:register 1] ([:dec 1] [:dec 1])]
         (run p/term "REG1 DEC DEC")))
  (is (= '[:term 4660 ([:cpl 1] [:dec 1])]
         (run p/term "1234 CPL DEC")))
  (is (= [:term 0x3FFF] (run p/term "3FFF")))
  (is (= '[:term :FA ([:shr 3])] (run p/term "FA SHR 3")))
  (is (= '[:term [:register 0x0A]] (run p/term "REGA")))
  ;; Should not consume beyond what it can
  (is (= '[:term [:register 0x0A]] (run p/term "REGA OR"))))

(deftest test-duop*
  (prn (run p/duop* "REGA OR"))
)

(deftest test-expr
  (prn (run p/expr "REGA AND 7F SHR INC OR FF")))

(deftest test-display
  (is (= [:display "HI"] (run p/display "DPY HI")))
  (is (= [:display "HI"] (run p/display "DPY-HI")))
  (is (= [:display "- HI"] (run p/display "DPY - HI")))
  (is (= [:display "#"] (run p/display "DPY-#")))
  (is (= [:display "SILENCING SOUND"] (run p/display "DPY-SILENCING SOUND"))))

(deftest test-program-decl
  (is (= :MAIN (run p/program-decl "PROGRAM MAIN 2 BYTES")))
  (is (= 0 (run p/program-decl "PROGRAM 0 2 BYTES")))
  (is (= :MAIN (run p/program-decl "PROGRAM MAIN")))
  (is (= 0 (run p/program-decl "PROGRAM 0"))))

(deftest test-execute
  (is (= [:execute :DELAY] (run p/execute "EX DELAY")))
  (is (= [:execute :DELAY] (run p/execute "EXECUTE DELAY")))
  (is (= [:execute :DELAY] (run p/execute "EXECUTE PROGRAM DELAY"))))

(deftest test-write
  (is (= [:write [:register 15] 0] (run p/write "WRITE @ REGF=00")))
  (is (= [:write 3145728 0] (run p/write "WRITE @ 300000 = 0")))
  (is (= [:write 3162111 17] (run p/write "WRITE @ 303FFF = 11")))
  (is (= [:write 2129920 0] (run p/write "WRITE @ 208000 = 0")))
  (is (= [:write 2131967 17] (run p/write "WRITE @ 2087FF = 11")))
  (is (= [:write 2131968 0] (run p/write "WRITE @ 208800 = 0")))
  (is (= [:write 2132991 17] (run p/write "WRITE @ 208BFF = 11")))
  (is (= [:write 2134016 0] (run p/write "WRITE @ 209000 = 0")))
  (is (= [:write 2135039 17] (run p/write "WRITE @ 2093FF = 11")))
  (is (= [:write 2136064 0] (run p/write "WRITE @ 209800 = 0")))
  (is (= [:write 2137087 17] (run p/write "WRITE @ 209BFF = 11"))))

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
            [:assign [:register 15] 26624]
            [:label :QUIET]
            [:write [:register 15] 0])]
         (run p/program "PROGRAM MAIN
   DPY-#
   DPY-SILENCING SOUND
   EX DELAY
   REGF=6800
QUIET:
   WRITE @ REGF=00
"))))

#_(deftest test-program-2
  (prn (run p/program "PROGRAM MAIN
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
   DPY-CPU LOCATION 0=4M  1=4J  2=4E \1
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

")))

#_(deftest test-parsing-source
  (prn (p/parse-string (slurp (io/resource "program.s")))))
