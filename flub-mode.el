;;; flub-mode.el --- Major mode for editing Fluke 9000 source

;; Copyright (C) 2014  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Keywords: extensions, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defconst flub-mode-font-lock-keywords
  `(
    ("\\b\\(REG\\)\\(.\\)\\b"
     (1 font-lock-builtin-face)
     (2 font-lock-variable-name-face))

    (,(format "\\b%s\\b"
              (regexp-opt '("INC" "IF" "DEC" "SIG" "RUN UUT" "SETUP"
                    "RAM" "GOTO" "EX" "EXECUTE"
                    "ROM" "IO" "POD")))
     . font-lock-keyword-face)

    ("STOP" . font-lock-warning-face)

    ("\\bINCLUDE\\b" . font-lock-preprocessor-face)

    ("\\b\\(DPY-?\\)\\(.*\\)"
     (1 font-lock-keyword-face t)
     (2 font-lock-string-face t))

    ;; Labels
    ("^\\s-*\\([A-Z0-9]+\\):" . font-lock-function-name-face)

    ("\\(PROGRAM\\)\s+\\([^\s-]+\\)" (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ("\\b[0-9A-F]+\\b" . font-lock-constant-face)

    ("@" . font-lock-constant-face)

    ("\\(!+\\)\\(.*\\)" (1 font-lock-comment-delimiter-face)
     (2 font-lock-comment-face t))
    ))

(define-derived-mode flub-mode asm-mode "f9k"
  "Major mode for editing Fluke 9000 source"
  (setq asm-comment-char ?!)
  (setq tab-width 2)
  (setq tab-stop-list '(2 0))
  (set (make-local-variable 'font-lock-defaults)
       '(flub-mode-font-lock-keywords)))


(provide 'flub-mode)
;;; flub-mode.el ends here
