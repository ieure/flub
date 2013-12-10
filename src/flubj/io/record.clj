;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flubj.io.record
  (:refer-clojure :exclude [char comment long newline])
  (:use [the.parsatron]
        [flubj.io.bytes :only [merge-bytes bytes->string]])
  (:import [the.parsatron ParseError Continue]))

(def any (token (fn [& _] true)))
(def long (token #(instance? Long %)))



(def ^:const trap-syms
  {2r00000001 :trap-data-err
   2r00000010 :trap-addr-err
   2r00000100 :trap-ctl-err
   2r00001000 :trap-active-force-line
   2r00010000 :trap-active-interrupt
   2r00100000 :trap-illegal-address
   2r10000000 :trap-bad-power-supply})

(defn traps [bits]
  (->> trap-syms
       (map (fn [[mask kw]] [kw (= mask (bit-and bits mask))]))
       (into {})))

(def trap
  (let->> [mask (>> (token #{0x01}) long)
           _ (eof)]
          (always [:trap (traps mask)])))



(def forcing-lines-mask
  (let->> [mask  (>> (token #{0x02}) long)
           _ (eof)]
          (always [:trap-forcing-lines mask])))

(defparser pboolean-value [marker kw]
  (let->> [val (>> (token #{marker}) long)
           _ (eof)]
          (always [kw (= val 0x01)])))

(defparser p32bit-value [marker kw]
  (let->> [ns (>> (token #{marker}) (times 4 long))
           _ (eof)]
          (always [kw (merge-bytes ns)])))



(def beep-on-error (pboolean-value 0x03 :beep-on-error))
(def exercise-errors (pboolean-value 0x04 :exercise-errors))
(def bus-test-addr (p32bit-value 0x05 :bus-test-addr))
(def run-uut-addr (p32bit-value 0x06 :run-uut-addr))
(def stall (p32bit-value 0x07 :stall))
(def unstall (p32bit-value 0x08 :unstall))
(def line-size (p32bit-value 0x09 :line-size))
(def timeout (p32bit-value 0x0A :timeout))
(def newline (p32bit-value 0x0B :newline))

(def pod-name
  (let->> [chars (>> (token #{0x0C}) (times 7 long))
           _ (eof)]
          (always [:pod (bytes->string chars)])))

(defn reserved []
  (fn [{:keys [pos] :as state} cok cerr eok eerr]
    (eerr (expect-error "Cannot parse reserved record." pos))))

;; FIXME
(def forcing-lines-enable-mask
  (always :FIXME))

(defn forcing-line-names* [marker kw]
  (let->> [name-bytes (>> (token #{marker}) (times 28 long))
           _ (eof)]
          (always [kw (mapv bytes->string (partition 7 name-bytes))])))

(def forcing-line-names-1 (forcing-line-names* 0x0E :forcing-line-names-1))
(def forcing-line-names-2 (forcing-line-names* 0x0F :forcing-line-names-2))



(def address-descriptor
  (let->> [_ (token #{0x19})
           low (times 4 long)
           hi (times 4 long)
           type long
           pad (times 5 long)
           sig-or-mask (times 4 long)
           _ (eof)]
          (let [low (merge-bytes low)
                hi (merge-bytes hi)
                sig-or-mask (merge-bytes sig-or-mask)]
            (always (cond
                     (and (= type 0x01) (= low hi)) [:io low sig-or-mask]
                     (= type 0x01) [:io low hi sig-or-mask]
                     (= type 0x02) [:ram low hi]
                     (= type 0x03) [:rom low hi sig-or-mask])))))

(def prognum
  (let->> [n (>> (token #{0x1A}) long)
           _ (eof)]
          (always [:program n])))



(def eos
  "End of string marker.

   According to the 9010 Programming manual, p. 7-5:

   '7C indicates the end of the text string in a Display or AUX I/F
   program step.'

   However, this seems not to be the case, as every compiled program I
   could get my hands on used 0x74. To be on the safe side, we match
   on either."
  (token #{0x7C 0x74}))

(def fluke-ascii-char
  (token (set (map #(bit-xor % 2r10000000) (range 0x20 0x7f)))))

(def displ
  (let->> [cs (many1 fluke-ascii-char)
           _ eos]
          (always ["DISPL" (bytes->string cs)])))



(def program-table
  {0x00 (always "0")
   0x01 (always "1")
   0x02 (always "2")
   0x03 (always "3")
   0x04 (always "4")
   0x05 (always "5")
   0x06 (always "6")
   0x07 (always "7")
   0x08 (always "8")
   0x09 (always "9")
   0x0A (always "A")
   0x0B (always "B")
   0x0C (always "C")
   0x0D (always "D")
   0x0E (always "E")
   0x0F (always "F")

   0x10 (always "LEARN")
   0x11 (always "RAM VIEW")
   0x12 (always "I/O VIEW")
   0x13 (always "ROM VIEW")
   0x14 (always "AUTO TEST")
   0x15 (always "BUS TEST")
   0x16 (always "ROM TEST")
   0x17 (always "RAM LONG")
   0x18 (always "RAM SHORT")
   0x19 (always "IO TEST")
   0x1A (always "PRIOR")
   0x1B (always "MORE")
   0x1C (always "ENTER/YES")
   0x1D (always "CLEAR/NO")
   0x1E (always "STS/CTL")
   0x1F (always "READ")
   0x20 (always "WRITE")
   0x21 (always "RAMP")
   0x22 (always "WALK")
   0x23 (always "TOGGL ADDR")
   0x24 (always "TOGGL DATA")
   0x25 (always "CONT")
   0x26 (always "RPEAT")
   0x27 (always "LOOP")
   0x28 (always "STOP")
   0x29 (always "RUN UUT")
   0x2A (always "PROGM")
   0x2B (always "LABEL")
   0x2C (always "GOTO")
   0x2D (always "IF")
   0x2E (always ">")
   0x2F (always "=")
   0x30 (always "AND")
   0x31 (always "OR")
   0x32 (always "SHIFT LEFT")
   0x33 (always "SHIFT RIGHT")
   0x34 (always "INCR")
   0x35 (always "DECR")
   0x36 (always "COMPL")
   0x37 (always "EXEC")
   0x38 (always "REG")
   0x39 (always "READ PROBE")
   0x3A (always "READ TAPE")
   0x3B (always "WRITE TAPE")
   0x3C (always "SYNC")
   0x3D (always "SETUP")
   0x3E displ
   0x3F (always "AUX I/F")

   0x44 (always "REG")
   })

(defn eop? "End of program?" [i]
  (= i 0x50))

(def sop "Start of program marker" (token #{0x53}))
(def eop "End of program marker" (token #{0x50}))

(def instruction
  (let->> [i (token (set (keys program-table)))]
          (get program-table i)))

(defn program-body*
  ([] (program-body* []))
  ([acc]
     (fn [state cok cerr eok eerr]
       (let->> [i (either instruction eop)]
               (if (eop? i)
                 (always (vec acc))
                 (Continue. #((program-body* (concat acc (get program-table i)))
                              state cok cerr eok eerr)))))))

(def label
  (let->> [n long
           lh (times 2 long)]
          (always [:label n (merge-bytes lh)])))

(def program-body
  (let->> [b (program-body*)
           end (token #{0x50})
           labels (many label)]
          (always nil)))

(def eosc (always nil))



(def parser-map
  {0x01 trap
   0x02 forcing-lines-mask
   0x03 beep-on-error
   0x04 exercise-errors
   0x05 bus-test-addr
   0x06 run-uut-addr
   0x07 stall
   0x08 unstall
   0x09 line-size
   0x0A timeout
   0x0B newline
   0x0C pod-name
   0x0D forcing-lines-enable-mask
   0x0E forcing-line-names-1
   0x0F forcing-line-names-2
   0x10 reserved
   0x11 reserved
   0x12 reserved
   0x13 reserved
   0x14 reserved
   0x15 reserved
   0x16 reserved
   0x17 reserved
   0x19 address-descriptor
   0x1A prognum
   0x53 program-body
   0x00 eosc})

(def record
  (let->> [record-type (lookahead (token (set (keys parser-map))))]
          (get parser-map record-type)))



(defn bytes->record [record-bytes]
  (run record record-bytes))

(defn bytes->records [records]
  (mapv bytes->record records))