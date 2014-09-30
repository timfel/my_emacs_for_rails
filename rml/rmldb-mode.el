(require 'gud)
(provide 'rmldb)

;; ======================================================================
;; rmldb functions

;;; History of argument lists passed to rmldb.
(defvar gud-rmldb-history nil)

(if (fboundp 'gud-overload-functions)
    (defun gud-rmldb-massage-args (file args)
      ())
  (defun gud-rmldb-massage-args (file args)
      ()))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defvar gud-rmldb-marker-acc "")
(make-variable-buffer-local 'gud-rmldb-marker-acc)

(defvar gud-rmldb-marker-regexp
  ;; a colon is correct here.
  "at \\([^:\\n]*\\):\\([0-9]*\\)")

(defun gud-rmldb-marker-filter (string)
  (setq gud-rmldb-marker-acc (concat gud-rmldb-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (message "match-for:%s" gud-rmldb-marker-regexp)
    (while (string-match gud-rmldb-marker-regexp
			 gud-rmldb-marker-acc)
      (setq
       ;; Extract the frame position from the marker.
       gud-last-frame
       (cons (substring gud-rmldb-marker-acc (match-beginning 1) (match-end 1))
	     (string-to-int (substring gud-rmldb-marker-acc
				       (match-beginning 2)
				       (match-end 2))))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
                      (substring gud-rmldb-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       gud-rmldb-marker-acc (substring gud-rmldb-marker-acc (match-end 0))))
    
    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-rmldb-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "t \\'" gud-rmldb-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring gud-rmldb-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq gud-rmldb-marker-acc
		(substring gud-rmldb-marker-acc (match-beginning 0))))

      (setq output (concat output gud-rmldb-marker-acc)
	    gud-rmldb-marker-acc ""))

       output))

(defun gud-rmldb-find-file (f)
  (save-excursion
    (let ((buf (find-file-noselect f)))
      (set-buffer buf)
;;      (gud-make-debug-menu)
      buf)))

(defvar rmldb-command-name "rmldb"
  "File name for executing rmldb.")

;;;###autoload
(defun rmldb (command-line)
  "Run rmldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (read-from-minibuffer "Run rmldb (like this): "
			       (if (consp gud-rmldb-history)
				   (car gud-rmldb-history)
				 (concat rmldb-command-name " "))
			       nil nil
			       '(gud-rmldb-history . 1))))

  (if (not (fboundp 'gud-overload-functions))
      (gud-common-init command-line 'gud-rmldb-massage-args
		       'gud-rmldb-marker-filter 'gud-rmldb-find-file)
    (gud-overload-functions '((gud-massage-args . gud-rmldb-massage-args)
			      (gud-marker-filter . gud-rmldb-marker-filter)
			      (gud-find-file . gud-rmldb-find-file)))
    (gud-common-init command-line rmldb-command-name))

  (gud-def gud-break  "break on %f:%l"         "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "break off %f%l"     "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "step"            "\C-s" "Step one source line with display.")
  (gud-def gud-next   "step"            "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "run"            "\C-r" "Continue with display.")
  (gud-def gud-finish "step"       "\C-f" "Finish executing current function.")
  (gud-def gud-up     "bt"        "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down   "bt"      ">" "Down N stack frames (numeric arg).")

  (setq comint-prompt-regexp "^rmldb@>")
  (if (boundp 'comint-last-output-start)
      (set-marker comint-last-output-start (point)))
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (run-hooks 'rmldb-mode-hook)
  )
