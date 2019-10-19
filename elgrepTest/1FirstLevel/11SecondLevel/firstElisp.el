;;; firstElisp.el --- just an example                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Tobias Zawada

;; Author: Tobias Zawada <i@tn-home.de>
;; Keywords: data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; File for testing elgrep.

;;; Code:

(defun firstElisp-demo1 ()
  "Just a demo for finding symbol references in functions."
  (message "A message call."))

(defvar firstElisp-myvar (progn
		(message "This message call should not be listed.")
		t)
  "Not a function definition.")

(defun firstElisp-fun-wo-message ()
  "This function should not be listed as match."
  (forward-sexp)
  (point))

(defun firstElisp-elgrep/process-options (option-defaults body)
  "Modified copy of elgrep/process-options with args OPTION-DEFAULTS and BODY."
  (setq option-defaults (cl-copy-list option-defaults))
  (while
      (cl-loop for opt on option-defaults by #'cddr
	       when (eq (car body) (car opt))
	       do (setf (cadr opt) (cadr body)
			body (cddr body))
	       and return t
	       finally return nil))
  (message "firstElisp-elgrep/process-options done.")
  (list option-defaults body))

(provide 'firstElisp)
;;; firstElisp.el ends here
