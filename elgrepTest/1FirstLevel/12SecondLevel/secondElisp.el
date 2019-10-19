;;; secondElisp.el --- Just a test for elgrep        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  DREWOR020

;; Author: DREWOR020 <toz@smtp.1und1.de>
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

;;

;;; Code:

;; Example for elgrep data:
(defvar elgrep-call-list (quote (("" elgrep "/home/me/Work/" "2019-\\(0[98]\\|10\\).*\\.org\\'" "octave") ("BibTeX QUERY" elgrep "~/References" "\\.bib\\'" ("^ *title *= *\\!(mech\\!)") :recursive t :r-beg "^@[[:alpha:]]+") ("" elgrep "~/KnowledgeDataBase" "\\.\\(tex\\|bib\\|org\\)\\'" "shape" :recursive t :async t) ("WORK" elgrep "/home/me/Work/" "2019-09-.*\\.org\\'" "") ("Samples" elgrep "/home/me/Examples/" "\\.model\\'" "\\.Components") ("TeX, Bib and ORG files in KnowledgeDataBase" elgrep "~/KnowledgeDataBase" "\\.\\(tex\\|bib\\|org\\)\\'" "" :recursive t :async t) ("Elisp in KnowledgeDataBase" elgrep "~/KnowledgeDataBase/Soft/Emacs" "\\.el\\'" "")))
  "Example for the content of file elgrep-data.el.")

(defmacro secondElisp-edit-advice (fun)
  "Define elgrep-FUN-function and elgrep-FUN-ad for FUN.
Advice FUN with elgrep-FUN-ad such that it calls
the function registered at elgrep-FUN-function if that variable is non-nil."
  (let ((elgrep-fun-function (intern (format "elgrep-%s-function" fun)))
	(elgrep-fun-ad (intern (format "elgrep-%s-ad" fun))))
    `(progn
       (defvar-local ,elgrep-fun-function (symbol-function (quote ,fun))
	 ,(format "Called to do the work of `%s' if non-nil." fun))

       (defun ,elgrep-fun-ad (fun &rest args)
	 ,(format "Call FUN with ARGS if `elgrep-%s-function' is nil.
If `elgrep-%s-function' is non-nil
call that function with ARGS instead." fun fun)
	 (if ,elgrep-fun-function
	     (apply ,elgrep-fun-function args)
	   (apply fun args)))
       (advice-add (quote ,fun) :around (function ,elgrep-fun-ad)))))

(defun secondElisp-test (&optional n)
  "Some doc string."
  (interactive "p")
  (let ((next-error-highlight next-error-highlight-no-select))
    (next-error n t))
  (message "Message to be found.")
  (pop-to-buffer next-error-last-buffer))

(provide 'secondElisp)
;;; secondElisp.el ends here
