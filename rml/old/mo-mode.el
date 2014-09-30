;;; rml-mode.el. Major mode for editing RML
;;;
;;; AUTHOR: Mikael Pettersson, mikpe@ida.liu.se, mikpe@inria.sophia.fr
;;; Ideas and code borrowed from sml-mode-3.2.
;;; Contributions from David Kågedal, x97davka@ida.liu.se.
;;;
;;; HISTORY
;;; 17-March-98 mikpe
;;;	New indentation code from davka.
;;;	`fail' is a new keyword.
;;; 08-March-98 mikpe
;;;	Made rml-insert-dashes behave correctly when at end of buffer.
;;; 27-Feb-98 mikpe
;;;	Finally replaced hilit19 stuff with font-lock.
;;;	Added davka's compilation-error-regexp-alist.
;;;	Added davka's indentation code.
;;; 18-Feb-98 mikpe
;;;	Removed `abstype' and `exists' keywords.
;;;	Added `eqtype' and `let' keywords.
;;; 02-April-96 mikpe
;;;	Removed `interface' keyword. Added `default' keyword.
;;; 09-February-96 mikpe
;;;	Made test for emacs19 more portable.
;;; 31-March-95 mikpe
;;;	Added `val' keyword.
;;; 16-March-95 mikpe
;;;	Created.

(provide 'mo-mode)

(defconst rml-mode-version-string
  "MODELICA-MODE, Version 0.6 (17 March 1998), mikpe@ida.liu.se")

;;; key bindings

(defvar rml-mode-map nil "The mode map used in rml-mode.")
(if rml-mode-map
    ()
  (setq rml-mode-map (make-sparse-keymap))
  (define-key rml-mode-map "\177" 'backward-delete-char-untabify)
  (define-key rml-mode-map "\C-c\C-v" 'rml-mode-version)
  (define-key rml-mode-map "\C-c-" 'rml-insert-dashes)
  (define-key rml-mode-map "\C-c&" 'rml-insert-&))

;;; syntax table

(defvar rml-mode-syntax-table nil "The syntax table used in rml-mode.")
(if rml-mode-syntax-table
    ()
  (setq rml-mode-syntax-table (make-syntax-table))
  ;; Set everything to be "." (punctuation) except for [A-Za-z0-9],
  ;; which will default to "w" (word-constituent).
  (let ((i 0) (len (length rml-mode-syntax-table)))
    (while (< i len)
      (modify-syntax-entry i "." rml-mode-syntax-table)
      (setq i (1+ i))))
  (let ((i ?0))
    (while (<= i ?9)
      (modify-syntax-entry i "w" rml-mode-syntax-table)
      (setq i (1+ i))))
  (let ((i ?A))
    (while (<= i ?Z)
      (modify-syntax-entry i "w" rml-mode-syntax-table)
      (setq i (1+ i))))
  (let ((i ?a))
    (while (<= i ?z)
      (modify-syntax-entry i "w" rml-mode-syntax-table)
      (setq i (1+ i))))
  ;; Now we change the characters that are meaningful to us.
  (modify-syntax-entry ?\( "()1" rml-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4" rml-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" rml-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" rml-mode-syntax-table)
  (modify-syntax-entry ?\* ". 23" rml-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" rml-mode-syntax-table)
  (modify-syntax-entry 32 " " rml-mode-syntax-table)
  (modify-syntax-entry ?\t " " rml-mode-syntax-table)
  (modify-syntax-entry ?\n " " rml-mode-syntax-table)
  (modify-syntax-entry ?\f " " rml-mode-syntax-table)
  (modify-syntax-entry ?\' "w" rml-mode-syntax-table)
  (modify-syntax-entry ?\_ "w" rml-mode-syntax-table)
  (modify-syntax-entry ?\. "w" rml-mode-syntax-table)	; was: "_"
  (modify-syntax-entry ?\\ "\\" rml-mode-syntax-table))

;;; compilation

(require 'compile)
(setq compilation-error-regexp-alist
      (cons
       '("\n\\([^:\n]+\\):\\([0-9]+\\).\\([0-9]+\\)" 1 2 3)
       compilation-error-regexp-alist))

;;; font-lock

(defvar rml-mode-font-lock-keywords
  '(("\\(function\\|model\\|case\\|of\\|record\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("\\(\\(data\\|union\\)?type\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (3 font-lock-variable-name-face))
    ("\\(--*\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("\\<\\(local\\|of\\|in\\|algorithm\\|end\\|else\\|assert\\|not\\|fail\\)\\>"
     (1 font-lock-keyword-face))))

;;;

(defun mo-mode ()
  "Major mode for editing RML code.

\\{rml-mode-map}
Entry to this mode runs the hooks in rml-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (rml-mode-variables)
  (use-local-map rml-mode-map)
  (setq major-mode 'mo-mode)
  (setq mode-name "Modelica")
  (run-hooks 'rml-mode-hook))

(defun rml-mode-variables ()
  (set-syntax-table rml-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'rml-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+[ \t]?")
  (make-local-variable 'comment-ident-function)
  (setq comment-indent-function 'rml-comment-indent)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(rml-mode-font-lock-keywords nil nil nil nil)))

;;; procedures

(defun rml-comment-indent ()
  (if (looking-at "^(\\*")		; Existing comment at beginning
      0					; of line stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	; Else indent at comment column
           comment-column))))		; except leave at least one space.

(defun rml-mode-version ()
  "The version of rml-mode."
  (interactive)
  (message rml-mode-version-string))

(defconst rml-decl-keyword
  "\\<\\(relation\\|and\\|end\\|module\\|\\(data\\)?type\\|val\\)\\>")

(defun rml-find-start ()
  (if (re-search-backward "\\<\\(relation\\|and\\|module\\|end\\)\\>" nil t)
      (let ((s (match-string 0)))
	(if (string= s "end")
	    (progn
	      (rml-find-start)
	      (rml-find-start))
	  (cons s (current-indentation))))
    (cons nil 0)))

(defun rml-indent-line ()
  (let ((case-fold-search nil))
    (save-excursion
      (back-to-indentation)
      (let ((col (save-excursion
		   (condition-case e
		       (progn
			 (backward-up-list 1)
			 (current-column))
		     (error nil)))))
	(if col
	    (indent-line-to (1+ col))
	  (cond ((looking-at "end\\>")
		 (indent-line-to (cdr (save-excursion (rml-find-start)))))
		((looking-at rml-decl-keyword)
		 (indent-line-to (save-excursion
				   (if (re-search-backward rml-decl-keyword
							   nil t)
				       (cond ((looking-at "module") 2)
					     (t (current-indentation)))
				     0))))
		((looking-at "\\(rule\\|axiom\\)\\>")
		 (indent-line-to 2))
		((looking-at "=>")
		 (indent-line-to 10))
		((looking-at "|")
		 (let ((col (save-excursion
			      (and (zerop (forward-line -1))
				   (re-search-forward "[\n]*[=|]" nil t)
				   (current-column)))))
		   (if col
		       (indent-line-to (1- col)))))
		;; ((looking-at "*")
		;;  (indent-line-to (save-excursion
		;; 		      (if (zerop (forward-line -1))
		;; 			  (if (looking-at "\\s-*(\\*")
		;; 			      (1+ (current-indentation))
		;; 			    (current-indentation))
		;; 			0))))
		(t
		 (let ((relcol (save-excursion
				 (if (and (re-search-backward
					   "\\<\\(relation\\|end\\)\\>" nil t)
					  (looking-at "relation"))
				     (current-column)
				   nil))))
		   (indent-line-to
		    (+ (or relcol 0)
		       (save-excursion
			 ;; Look for special first words
			 (if (re-search-backward "^.*\\S-.*$" nil t)
			     (cond ((looking-at "\\s-*rule") 8)
				   ((looking-at "\\s-*module") 2)
				   ((looking-at "\\s-*=>") 10)
				   (t (if relcol 8 0)))
			   0))))))))))
    (cond ((re-search-backward "^[ \t]*\\=" nil t)
	   (back-to-indentation))
	  ((save-excursion
	     (re-search-backward "\\(rule\\|axiom\\)[ \t]*\\=" nil t))
	   (delete-horizontal-space)
	   (tab-to-tab-stop)))))

(defun rml-insert-dashes ()
  "Inserts \\n\\t----------------\\n\\t"
  (interactive)
  (back-to-indentation)
  (if (looking-at "rule\\s-+")
      (goto-char (match-end 0)))
  (let ((c (current-column))
	l)
    (end-of-line)
    (setq l (- (current-column) c))
    (newline)
    (move-to-column c t)
    (insert (make-string l ?-))
    (if (= (forward-line 1) 1)	; if at end of buffer, add a line
	(newline))
    (move-to-column c t)))

(defun rml-insert-& ()
  "Inserts SPC, &, \\n, \\t"
  (interactive)
  (let ((ch (char-after (1- (point)))))
    (if (and ch (/= ch ?\ ))
	(insert " ")))
  (insert "&")
  (newline-and-indent))
