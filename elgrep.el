;;; elgrep.el --- Searching files for regular expressions; (emacs-lisp implementation) -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Tobias Zawada

;; Author: Tobias Zawada <naehring@smtp.1und1.de>
;; Keywords: tools, matching, files, unix

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

;; Open the `elgrep-menu' via menu item "Tools" -> "Search files (Elgrep)...".
;; There are menu items for the directory, the file name regexp for filtering
;; and the regexp for grepping. Furthermore, you can also switch on recursive grep.
;;
;; Run M-x elgrep to search a single directory for files with file
;; name matching a given regular expression for text matching a given
;; regular expression. With prefix arg it searches the directory
;; recursively.

;;; Code:

(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(require 'cl-lib)
(require 'easymenu)

(defvar elgrep-file-name-re-hist nil
  "History of file-name regular expressions for `elgrep' (which see).")
(defvar elgrep-re-hist nil
  "History of regular expressions for `elgrep' (which see).")

(defun elgrep-insert-file-contents (filename &optional visit)
  "Like `insert-file-contents' but uses `pdftotext' (poppler) for pdf-files (with file extension pdf)."
  (if (string-match (downcase filename) "\.pdf$")
      (call-process "pdftotext" filename (current-buffer) visit "-" "-")
    (insert-file-contents filename visit)))

(defun elgrep-dired-files (files)
  "Print files in `dired-mode'"
  (insert "  " default-directory ":\n  elgrep 0\n")
  (dolist (file files)
    (let ((a (file-attributes file 'string)))
      (insert (format "  %s %d %s %s %6d %s %s\n"
		      (nth 8 a) ; file modes like ls -l
		      (nth 1 a) ; number of links to file
		      (nth 2 a) ; uid as string
		      (nth 3 a) ; gid as string
		      (nth 7 a) ; size in bytes
		      (format-time-string "%d. %b %Y" (nth 5 a)) ; modification time
		      file))))
  (dired-mode)
  (dired-build-subdir-alist))

(defmacro elgrep-line-position (num-or-re pos-op search-op)
  "If NUM-OR-RE is a number then act like (POS-OP (1+ NUM-OR-RE)) with POS-OP being `line-end-position' or `line-beginning-position'.
If num-or-re is a regular expression search for that RE and return line-end-position or line-beginning-position of the line with the match, respectively."
  `(if (stringp ,num-or-re)
       (save-excursion
	 (save-match-data
	   (,search-op ,num-or-re nil 'noErr)
	   (,pos-op)
	   ))
     (,pos-op (and ,num-or-re (1+ ,num-or-re)))))

(defun classify (classifier list &rest key-values)
  "Maps the LIST entries through CLASSIFIER to class denotators.
Returns the list of equivalence classes.  Each equivalence class
is a cons whose `car' is the class denotator and the cdr is the
list of members.

Keywords supported: :test"
  (let ((test (or (plist-get key-values :test) 'equal)))
    (let (classify-res)
      (dolist (classify-li list)
	(let* ((classify-key (funcall classifier classify-li))
	       (classify-class (cl-assoc classify-key classify-res :test test)))
	  (if classify-class
	      (setcdr classify-class (cons classify-li (cdr classify-class)))
	    (setq classify-res (cons (list classify-key classify-li) classify-res)))))
      classify-res)))

(defun elgrep-default-filename-regexp (&optional dir)
  "Create default filename regexp from the statistical analysis of files in DIR which defaults to `default-directory'."
  (unless dir (setq dir default-directory))
  (let* ((filelist (cl-delete-if (lambda (file) (string-match "\\.\\(~\\|bak\\)$" file))
				 (directory-files dir)))
	 (ext (car-safe (cl-reduce (lambda (x y) (if (> (length x) (length y)) x y)) (classify 'file-name-extension filelist))));; most often used extension
	 )
    (concat "\\." ext "$")))

(defun elgrep-menu-elgrep (&optional event)
  "Start `elgrep' with the start-button from `elgrep-menu'."
  (interactive)
  (declare (special w-dir w-file-name-re w-re w-recursive w-start w-cBeg w-cEnd w-exclude-file-re w-dir-re w-exclude-dir-re))
  (let ((pos (event-start event)))
    (with-current-buffer (window-buffer (posn-window pos))
      (elgrep (widget-value w-dir)
	      (widget-value w-file-name-re)
	      (widget-value w-re)
	      :recursive (widget-value w-recursive)
	      :cBeg (- (widget-value w-cBeg))
	      :cEnd (widget-value w-cEnd)
	      :exclude-file-re (widget-value w-exclude-file-re)
	      :dir-re (widget-value w-dir-re)
	      :exclude-dir-re (widget-value w-exclude-dir-re)
	      :interactive t))))

(defmacro elgrep-menu-with-buttons (buttons &rest body)
  "Execute body with keymaps for widgets.
BUTTONS is a list of button definitions (KEYMAP-NAME FUNCTION).
See definition of `elgrep-menu' for an example."
  (declare (debug (expr &rest form))
	   (indent 1))
  (append (list #'let (mapcar (lambda (button)
				(list (car button) '(make-sparse-keymap)))
			      buttons))
	  (mapcar (lambda (button)
		    `(progn
		       (define-key ,(car button) (kbd "<down-mouse-1>") ,(cadr button))
		       (define-key ,(car button) (kbd "<down-mouse-2>") ,(cadr button))
		       (define-key ,(car button) (kbd "<return>") ,(cadr button))))
		  buttons)
	  body))

(defun elgrep-menu (&optional reset)
  "Present a menu with most of the parameters for `elgrep'.
You can adjust the parameters there and start `elgrep'."
  (declare (special w-dir w-file-name-re w-re w-recursive w-start w-cBeg w-cEnd w-exclude-file-re w-dir-re w-exclude-dir-re))
  (interactive)
  (if (and (buffer-live-p (get-buffer "*elgrep-menu*"))
	   (null reset))
      (switch-to-buffer "*elgrep-menu*")
    (switch-to-buffer "*elgrep-menu*")
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (elgrep-menu-with-buttons ((elgrep-menu-start-map #'elgrep-menu-elgrep)
			       (elgrep-menu-bury-map #'bury-buffer)
			       (elgrep-menu-reset-map (lambda () (interactive) (elgrep-menu t))))
      (buffer-disable-undo)
      (let ((caption "Elgrep Menu"))
	(widget-insert (concat caption "\n" (make-string (length caption) ?=) "\n\n")))
      (setq-local w-re (widget-create 'editable-field :format "Regular Expression: %v" ""))
      (setq-local w-dir (widget-create 'editable-field :format "Directory: %v" default-directory))
      (setq-local w-file-name-re (widget-create 'editable-field :format "File Name Regular Expression: %v" (elgrep-default-filename-regexp default-directory)))
      (setq-local w-exclude-file-re (widget-create 'editable-field :format "Exclude File Name Regular Expression (ignored when empty): %v" ""))
      (setq-local w-dir-re (widget-create 'editable-field :format "Directory Name Regular Expression: %v" ""))
      (setq-local w-exclude-dir-re (widget-create 'editable-field :format "Exclude Directory Name Regular Expression (ignored when empty): %v" ""))
      (widget-insert  "Recurse into subdirectories ")
      (setq-local w-recursive (widget-create 'checkbox nil))
      (setq-local w-cBeg (widget-create 'number :format "\nContext Lines Before The Match: %v" 0))
      (setq-local w-cEnd (widget-create 'number :format "Context Lines After The Match: %v" 0))
      (widget-insert "\n")
      (widget-create 'push-button (propertize "Start elgrep" 'keymap elgrep-menu-start-map 'mouse-face 'highlight 'help-echo "<down-mouse-1> or <return>: Start elgrep with the specified parameters"))
      (widget-insert " ")
      (widget-create 'push-button (propertize "Burry elgrep menu" 'keymap elgrep-menu-bury-map 'mouse-face 'highlight 'help-echo "<down-mouse-1> or <return>: Bury elgrep-menu."))
      (widget-insert " ")
      (widget-create 'push-button (propertize "Reset elgrep menu" 'keymap elgrep-menu-reset-map 'mouse-face 'highlight 'help-echo "<down-mouse-1> or <return>: Reset elgrep-menu.")))
    (use-local-map widget-keymap)
    (local-set-key "q" #'bury-buffer)
    (widget-setup)))

(defun elgrep-get-formatter ()
   "Returns a formatter for elgrep-lines.
The formatter is a function with two arguments FNAME and PART.
FNAME is the file name where the match occurs. PART is a property list with members
:match (the actual match)
:context (the match including context lines)
:line (the line in the source code file)
:line-beg (the beginning position of the context in the source code file)
:beg (the beginning position of the match)
:end (the end position of the match)
The formatter is actually a capture that remembers the last file name and the line number
such that the same line number is not output multiple times."
   (let ((last-file "")
	 (last-line 0)
	 (output-beg 0))
     (lambda (fname part)
       (let ((line (plist-get part :line)))
	 (unless (and (string-equal last-file fname)
		      (= last-line line))
	   (insert (format "%s:%d:" fname line))
	   (setq output-beg (point))
	   (insert (plist-get part :context) ?\n))
	 (let* ((context-beg (plist-get part :context-beg))
		(match-beg (+ (- (plist-get part :beg) context-beg) output-beg))
		(match-end (+ (- (plist-get part :end) context-beg) output-beg)))
	   (when (and (>= match-beg output-beg)
		      (<= match-end (point-max)))
	     (put-text-property match-beg match-end 'font-lock-face 'match)))
	 (setq last-file fname
	       last-line line)
	 ))))

(defun elgrep (dir file-name-re re &rest options)
  "Grep files via emacs lisp (no dependence on external grep).
Return list of filematches.

Each filematch is a cons (file . matchdata).
file is the file name.
matchdata is a list of matches.
Each match is a list of sub-matches.
Each submatch is a plist of :match, :context, :line, :linestart, :beg and :end.

options is a plist
Flags:

:abs absolute file names
t: full absolute file names;
nil: (default) file names relative to default-directory of the last visited buffer

:interactive
t: call as interactive

:cBeg context begin (line beginning)
Lines before match defaults to 0. Can also be a regular expression.
Then this re is searched for in backward-direction starting at the current elgrep-match.

:cEnd context end (line end)
Lines behind match defaults to 0
Then this re is searched for in forward-direction starting at the current elgrep-match.

:cOp
Context operation gets beginning and end position of context as arguments. Defaults to buffer-substring-no-properties

:recursive
t: also grep recursively subdirectories in dir (also if called interactively with prefix arg)

:formatter
Formatting function to call for each match if called interactively with non-nil RE.
Inputs: format string \"%s:%d:%s\n\", file-name, line number,

:exclude-file-re
Regular expression matching the files that should not be grepped.

:dir-re
Regular expression matching the directories that should be entered in recursive grep.
Defaults to \"\".

:exclude-dir-re
Regular expression matching the directories that should not be entered in recursive grep.
If this is the empty string no directories are excluded.
Defaults to \"^\\.\".

:keep-elgrep-buffer
Keep buffer <*elgrep*> even when there are no matches.
"
  (interactive (let ((dir (read-directory-name "Directory:")))
		 (append (list dir
			       (let ((default-file-name-regexp (elgrep-default-filename-regexp dir)))
				 (read-regexp (concat "File-name regexp (defaults:\"\" and \"" default-file-name-regexp "\"):") (list "" default-file-name-regexp) 'elgrep-file-name-re-hist)
				 )
			       (read-regexp "Emacs regexp:" nil 'elgrep-re-hist))
			 (when current-prefix-arg (list :recursive t)))))
  (when (and (stringp re) (= (length re) 0))
    (setq re nil))
  (unless dir
    (setq dir default-directory))
  (setq dir (expand-file-name (directory-file-name (substitute-in-file-name dir))))
  (with-current-buffer (get-buffer-create "*elgrep*")
    (buffer-disable-undo)
    (setq default-directory dir)
    (let ((files (directory-files dir (plist-get options :abs) file-name-re))
	  (formatter (or (plist-get options :formatter)
			 (elgrep-get-formatter)))
	  filematches
	  (inhibit-read-only t)
	  (cOp (or (plist-get options :cOp) 'buffer-substring-no-properties))
	  (exclude-file-re (plist-get options :exclude-file-re)))
      (when (and exclude-file-re (null (string-equal exclude-file-re "")))
	(setq files (cl-remove-if (lambda (fname) (string-match exclude-file-re fname)) files)))
      (cl-loop for file in files do
	    (when (file-regular-p file)
	      (if re
		  (progn
		    (erase-buffer)
		    (elgrep-insert-file-contents (if (plist-get options :abs) file (expand-file-name file dir)))
		    (let (filematch
			  (last-line-number 1)
			  (last-pos (point-min)))
		      (goto-char (point-min))
		      (while (search-forward-regexp re nil 'noErr)
			(let* ((n (/ (length (match-data)) 2))
			       (matchdata (cl-loop for i from 0 below n
						   collect
						   (let ((context-beginning (elgrep-line-position (plist-get options :cBeg) line-beginning-position re-search-backward))
							 (context-end (elgrep-line-position (plist-get options :cEnd) line-end-position re-search-forward)))
						     (list :match (match-string-no-properties i)
							   :context (funcall cOp context-beginning context-end)
							   :line (prog1
								     (setq last-line-number
									   (+ last-line-number -1
									      (save-excursion
										(save-restriction
										  (narrow-to-region last-pos (point))
										  (goto-char (point-min))
										  (- (buffer-size) (forward-line (buffer-size)))))))
								   (setq last-pos (point)))
							   :context-beg context-beginning
							   :beg (match-beginning i)
							   :end (match-end i))))))
			  (setq filematch (cons matchdata filematch))))
		      (when filematch
			(setq filematches (cons (cons file (nreverse filematch)) filematches)))))
		;; no re given; just register file with dummy matchdata
		(setq filematches (cons (list file) filematches)))))
      (setq filematches (nreverse filematches))
      (when (plist-get options :recursive)
	(setq files (cl-loop for file in (directory-files dir)
			     if (and (file-directory-p (expand-file-name file dir))
				     (let ((dir-re (plist-get options :dir-re))
					   (exclude-dir-re (plist-get options :exclude-dir-re)))
				       (and (or (null dir-re)
						(string-match dir-re file))
					    (null
					     (and exclude-dir-re
						  (null (string-equal exclude-dir-re ""))
						  (string-match exclude-dir-re file)))))
				     (null (string-match "^\\.[.]?$" file)))
			     collect file))
	(dolist (file files)
	  (setq filematches
		(append
		 (if (plist-get options :abs)
		     (apply 'elgrep (expand-file-name file dir) file-name-re re :keep-elgrep-buffer t options)
		   (let ((files (apply 'elgrep (expand-file-name file dir) file-name-re re :keep-elgrep-buffer t options)))
		     ;;(debug)
		     (cl-loop for f on files do
			   (setcar f (cons (file-relative-name (expand-file-name (caar f) file)) (cdar f))))
		     files))
		 filematches))))
      (when (or (plist-get options :interactive) (called-interactively-p 'any))
	(cl-assert (string-equal (buffer-name) "*elgrep*") nil "Expected buffer <*elgrep*> got %s." (current-buffer))
	(if filematches
	    (progn
	      (unless (plist-get options :abs)
		(setq default-directory dir))
	      (delete-region (point-min) (point-max))
	      (if re
		  (progn
		    (dolist (filematch filematches)
		      (let ((fname (car filematch)))
			(dolist (match (cdr filematch))
			  (dolist (part match)
			    (funcall formatter fname part)
			    ))))
		    (grep-mode))
		(elgrep-dired-files (mapcar 'car filematches)))
	      (display-buffer (current-buffer)))
	  (unless (plist-get options :keep-elgrep-buffer)
	    (kill-buffer))
	  (message "elgrep: No matches for \"%s\" in files \"%s\" of dir \"%s\"." re file-name-re dir)))
      filematches)))

(easy-menu-add-item global-map '("menu-bar" "tools") ["Search Files (Elgrep)..." elgrep-menu t] "grep")

(provide 'elgrep)
;;; elgrep.el ends here
