;; -*- coding: utf-8 -*-
;;
;; © 2013 Buster Marx, Inc All rights reserved.
;; Author: Ian Eure <ian.eure@gmail.com>
;;
(ns flub.parser.lines "Lines, whitespace, and comments."
  (:refer-clojure :exclude [char comment])
  (:use [the.parsatron]
        [flub.parser.common]))

 ;; Whitespace & line handling

(def ws "Match one non-newline whitespace char. Don't use this directly."
  (>> (token #{\space \tab}) (always nil)))

(def anyws "Match any whitespace character."
  (>> (many (token #{\space \tab \newline})) (always nil)))

(def optws "Match 0 or more non-newline whitespace chars."
  (>> (many ws) (always nil)))

(def reqws "Match 1 or more non-newline whitespace characters."
  (>> (many1 ws) (always nil)))

(def comment "Match a comment at the end of a line. Don't use directly."
  (let->> [_ (>> optws (char \!) (many (butchar \newline)))]
          (always nil)))

(def eol "Match a single newline. Don't use directly."
  (>> (char \newline) (always nil)))

(def eol* "Match to EOL, including comment."
  (>> optws (many comment) (either eol (eof))))

(defparser line-of
  ;; "Match a line which contains `parser'.

  ;;  A line may contain leading and trailing whitespace, an optional
  ;;  comment, and MUST end with an EOL character."
  [parser]
  (let->> [_ optws
           r parser
           _ eol*]
          (always r)))
