;;; flub-mode.el --- Major mode for editing Fluke 9000 source

;; Copyright (C) 2014, 2015, 2016  Ian Eure

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
    ("\\b\\(REG\\)\\s-*\\([A-F0-9]\\)\\b"
     (1 font-lock-builtin-face)
     (2 font-lock-variable-name-face))

    (,(format "\\b%s\\b"
              (regexp-opt '("INC" "IF" "DEC" "SIG" "RUN UUT" "SETUP"
                            "RAM"  "ROM" "IO" "TRAP" "POD"
                            "READ" "RD" "WRITE" "STS"
                            "BUS TEST" "ADDRESS SPACE INFORMATION")))

     . font-lock-builtin-face)

    ("STOP" . font-lock-warning-face)

    ("\\b\\(YES\\|NO\\)\\b" . font-lock-constant-face)

    (,(format "\\b%s\\b"
              (regexp-opt '("ACTIVE FORCE LINE"
                            "ADDRESS ERROR"
                            "BAD POWER SUPPLY"
                            "CONTROL ERROR"
                            "DATA ERROR")))
     . font-lock-keyword-face)

    ("\\bINCLUDE\\b" . font-lock-preprocessor-face)

    ("\\b\\(\\(DPY\\|AUX\\)\\s-*-?\\)\\(.\\{0,27\\}\\)"
     (2 font-lock-builtin-face t)
     (3 font-lock-string-face t))

    ;; Labels
    ("^\\s-*\\([A-Z0-9]+\\):" . font-lock-function-name-face)

    ("\\(PROGRAM\\)\s+\\([0-9A-Z]+\\)" (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ("\\(EX\\|EXECUTE\\)\s+\\([0-9A-Z]+\\)" (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ("\\(GOTO\\)\s+\\([0-9A-Z]+\\)" (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ("\\b[0-9A-F]+\\b" . font-lock-constant-face)

    ("@" . font-lock-constant-face)

    ("\\(!+\\)\\(.*\\)" (1 font-lock-comment-delimiter-face)
     (2 font-lock-comment-face t))
    ))

(defun flub-indent-line ()
  "Auto-indent the current line."
  (interactive)

  (save-excursion
    (save-match-data
      (back-to-indentation)
      (indent-line-to
       (cond ((or (bobp)
                  (looking-at "\\(!\\|SETUP\\|PROGRAM\\|[0-9A-Z]+:\\)")) 0)
             ((looking-at "INCLUDE") (if (bobp) 0
                                       (forward-line -1)
                                       (current-indentation)))
             (t 2)))))
  (when (looking-at "\\s-+") (goto-char (line-end-position))))

(defvar flub-mode-syntax-table
  (let ((table asm-mode-syntax-table))
    (modify-syntax-entry table ?! "!")
    (modify-syntax-entry table ?< "!")))

(define-derived-mode flub-mode asm-mode "f9k"
  "Major mode for editing Fluke 9000 source"
  (setq tab-width 2)
  (setq tab-stop-list '(2 0))
  (setq indent-line-function 'flub-indent-line)
  (setq tab-always-indent t)

  (set-syntax-table flub-mode-syntax-table)
  (setq asm-comment-char ?!)
  (setq comment-use-syntax t)
  (setq fill-prefix nil)

  (set (make-local-variable 'font-lock-defaults)
       '(flub-mode-font-lock-keywords nil t)))

(provide 'flub-mode)
;;; flub-mode.el ends here
