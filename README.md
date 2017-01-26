Emacs-Lisp Grep (`elgrep`)
====
Short description
----
Package for searching text matching a given regular expression in files with names matching another regular expression within a user-specified directory.
The directory can optionally be searched recursively.
PDF-files can also be searched but for that purpose the external tool `pdftotext` (or some equivalent program) is required.

Installation
----
Save `elgrep.el` in one of the directories specified in the `laod-path` of emacs and
put the following line into your initialization file (e.g., `~/.emacs`):

`(require 'elgrep)`

Usage
----
Open the `elgrep-menu` via menu item "Tools" → "Search files (Elgrep)...".
There are menu items for the directory, the file name regexp for filtering
and the regexp for grepping. Furthermore, you can also switch on recursive grep.

Run <kbd>M-x</kbd> `elgrep` to search a single directory for files with file
name matching a given regular expression for text matching a given
regular expression. With prefix arg <kbd>C-u</kbd> it searches the directory
recursively.
