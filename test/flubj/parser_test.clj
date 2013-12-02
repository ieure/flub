;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flubj.parser-test
  (:use [clojure.test]
        [the.parsatron :only [run digit char >>]])
  (:require [flubj.parser :as p]
            [clojure.java.io :as io]))

(deftest test-mchoice
  (is (= \A (run (p/mchoice (char \A) (char \B)) "A")))
  (is (= \B (run (p/mchoice (char \A) (char \B)) "B")))
  (is (thrown? RuntimeException (run (p/mchoice (char \A) (char \B)) "0")))
  (is (= \A (run (p/mchoice (>> (char \A) (char \A))
                            (>> (char \A) (char \B)))
                 "AA")))
  (is (= \B (run (p/mchoice (>> (char \A) (char \A))
                            (>> (char \A) (char \B)))
                 "AB"))))

(deftest test-mchoice*
  (is (= [] (run (p/mchoice* (>> (char \A) (char \A))
                             (>> (char \A) (char \B))) "")))
  (is (= '(\A \A \A) (run (p/mchoice* (>> (char \A) (char \A))
                                      (>> (char \A) (char \B)))
                          "AAAAAA")))
  (is (= '(\B \B \B) (run (p/mchoice* (>> (char \A) (char \A))
                                      (>> (char \A) (char \B)))
                 "ABABAB"))))

(deftest test-mchoice+
  (is (thrown? RuntimeException
               (run (p/mchoice+ (>> (char \A) (char \A))
                                (>> (char \A) (char \B))) "")))
  (is (= '(\A \A \A) (run (p/mchoice+ (>> (char \A) (char \A))
                                      (>> (char \A) (char \B)))
                          "AAAAAA")))
  (is (= '(\B \B \B) (run (p/mchoice+ (>> (char \A) (char \A))
                                      (>> (char \A) (char \B)))
                          "ABABAB"))))

(deftest test-constants
  (is (= \A (run p/hex-char "A")))
  (is (= 255 (run p/hex-constant "FF")))
  (is (= 255 (run p/decimal-constant "255")))
  (is (= 42 (run p/binary-constant "101010"))))

(deftest test-register
  (is (= [:register 15] (run p/register "REGF")))
  (is (thrown? RuntimeException (run p/register "REGZ"))))

(deftest test-lines
  (is (nil? (run p/ws "  \t   \t ")))
  (is (thrown? RuntimeException (run p/ws "")))
  (is (nil? (run p/optws "")))
  (is (nil? (run p/optws "  \t   \t ")))
  (is (nil? (run p/reqws "  \t   \t ")))
  (is (thrown? RuntimeException (run p/reqws "")))

  ;; EOL
  (is (nil? (run p/eol "\n")))
  (is (thrown? RuntimeException (run p/eol "")))
  (is (nil? (run p/eol* "\n")))
  (is (nil? (run p/eol* "      \n")))
  (is (nil? (run p/eol* "  ! hey    \n")))
  (is (nil? (run p/eol* "")))

  ;; Lines
  (is (= \0 (run (p/line-of (digit)) "0\n")))
  (is (= \0 (run (p/line-of (digit)) "0  \n")))
  (is (= \0 (run (p/line-of (digit)) "  0\n")))
  (is (= \0 (run (p/line-of (digit)) "  0  \n")))
  (is (= \0 (run (p/line-of (digit)) "0 ! hey\n")))
  (is (= \0 (run (p/line-of (digit)) "0 ! hey \n")))
  (is (= \0 (run (p/line-of (digit)) "  0 ! hey\n")))
  (is (= \0 (run (p/line-of (digit)) "  0 ! hey \n")))
  (is (= \0 (run (p/line-of (digit)) "  0 !hey\n")))
  (is (= \0 (run (p/line-of (digit)) "  0 !hey \n")))
  (is (= \0 (run (p/line-of (digit)) "  0 !hey\n"))))

(deftest test-comments
  (is (nil? (run p/comment "      ! sup mang"))))

(deftest test-dash
  (is (nil? (run p/dash "-")))
  (is (nil? (run p/dash " -")))
  (is (nil? (run p/dash "- ")))
  (is (nil? (run p/dash " - "))))

(deftest test-yes-no
  (is (= true (run p/yes-no "YES")))
  (is (= false (run p/yes-no "NO"))))

(deftest test-address-range
  (is (= [0 65535] (run p/address-range "0000-FFFF")))
  (is (= [0 65535] (run p/address-range "0000 - FFFF"))))

(deftest test-sym
  (is (= :foo (run p/sym "foo")))
  (is (= :foo_bar (run p/sym "foo_bar")))
  (is (thrown? RuntimeException (run p/sym "10bar"))))

(deftest test-fluke-string
  (is (= "foo bar" (run p/fluke-string "\"foo bar\"")))
  (is (thrown? RuntimeException (run p/fluke-string "\"foo bar")))
  (is (thrown? RuntimeException (run p/fluke-string "'foo bar\"")))
  (is (thrown? RuntimeException (run p/fluke-string "'foo bar'"))))

(deftest test-term-unary-operator
  ;; Known broken
  #_(is (= [:inc 7] (run p/term-unary-operator "INC 7")))
  (is (= [:inc 1] (run p/term-unary-operator "INC"))))

(deftest test-term
  (is (= [:term [:register 10] '([:shr 1] [:inc 1])]
         (run p/term "REGA SHR INC")))
  (is (= [:term 4660 '([:cpl 1] [:dec 1])]
         (run p/term "1234 CPL DEC")))
  (is (= [:term 4660] (run p/term "1234 AND 4321")))
  )

#_(deftest test-expr
  (prn (run p/expr "REGA AND 7F SHR INC OR FF")))

(deftest test-trap
  (is (= {:data-error true} (run p/trap "TRAP DATA ERROR YES")))
  (is (thrown? RuntimeException
               (run p/trap "   TRAP CONTROL ERROR YES  \n")))
  (is (= {:control-error true}
         (run p/trap "TRAP CONTROL ERROR YES"))))

(deftest test-ram
  (is (= [:ram 0 65535] (run p/ram "RAM 0000-FFFF")))
  (is (= [:ram 0 65535] (run p/ram "RAM @ 0000-FFFF")))
  (is (= [:ram 0 65535] (run p/ram "RAM @0000-FFFF"))))

(deftest test-rom
  (is (= [:rom 0 65535 4660] (run p/rom "ROM 0000-FFFF SIG 1234")))
  (is (= [:rom 0 65535 4660] (run p/rom "ROM @ 0000-FFFF   SIG 1234")))
  (is (= [:rom 0 65535 4660] (run p/rom "ROM @0000-FFFF SIG 1234"))))

(deftest test-io
  (is (= [:io 0 65535 127] (run p/io "I/O 0000-FFFF BITS 7F")))
  (is (= [:io 0 65535 255] (run p/io "I/O @ 0000-FFFF   BITS FF")))
  (is (= [:io 0 65535 10] (run p/io "I/O @0000-FFFF BITS 0A"))))

(deftest test-address-space
  (is (= [:address-space {:ram [[0 8191] [12288 12799]],
                          :rom [[8192 12287 4660]]}]
         (run p/address-space "ADDRESS SPACE\nRAM @ 0000-1FFF\nRAM 3000-31FF\nROM @ 2000-2FFF SIG 1234")))
  (is (thrown? RuntimeException
               (run p/address-space "ADDRESS SPACE INFORMATION\n"))))

(deftest test-setup
  (is (= [:setup {:active-interrupt false}]
         (run p/setup "SETUP\nTRAP ACTIVE INTERRUPT NO")))
  (is (= [:setup nil] (run p/setup "SETUP INFORMATION !hi")))
  (is (= [:setup {:active-interrupt false
                  :data-error true}]
         (run p/setup "SETUP\n    TRAP ACTIVE INTERRUPT NO\n  TRAP DATA ERROR YES")))

  (is (= [:setup {:active-force-line false
                  :active-interrupt false}]
         (run p/setup "SETUP

   TRAP ACTIVE FORCE LINE NO
   TRAP ACTIVE INTERRUPT NO
"))))

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
