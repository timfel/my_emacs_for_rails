;;; timfel-agent-shell-vc.el --- Agent-shell context helpers for VC -*- lexical-binding: t -*-

;;; Commentary:

;; Turn the current built-in VC diff or vc-dir entry into `agent-shell'
;; context.

;;; Code:

(require 'diff-mode)
(require 'seq)
(require 'subr-x)
(require 'vc)
(require 'vc-dir)

(defun timfel/agent-shell-vc--stringify-list (values)
  "Return VALUES as a comma-separated string."
  (mapconcat (lambda (value) (format "%s" value)) values ", "))

(defun timfel/agent-shell-vc--format-context (backend revisions files patch)
  "Format VC diff context for `agent-shell'."
  (string-join
   (delq nil
         (list "[VC DIFF CONTEXT]"
               (when backend
                 (format "Backend: %s" backend))
               (when revisions
                 (format "Revisions: %s"
                         (timfel/agent-shell-vc--stringify-list revisions)))
               (when files
                 (format "Files: %s"
                         (timfel/agent-shell-vc--stringify-list files)))
               ""
               "```diff"
               patch
               "```"
               "[END CONTEXT]"))
   "\n"))

(defun timfel/agent-shell-vc--substring (bounds)
  "Return the buffer substring for BOUNDS."
  (buffer-substring-no-properties (car bounds) (cadr bounds)))

(defun timfel/agent-shell-vc--file-header-before (position)
  "Return the current diff file header before POSITION."
  (save-excursion
    (goto-char position)
    (let* ((hunk-start (car (diff-bounds-of-hunk)))
           (file-start (car (diff-bounds-of-file))))
      (buffer-substring-no-properties file-start hunk-start))))

(defun timfel/agent-shell-vc--diff-mode-patch ()
  "Return the current `diff-mode' file, hunk, or region patch."
  (when (derived-mode-p 'diff-mode)
    (condition-case nil
        (string-trim-right
         (cond
          ((region-active-p)
           (let ((beg (region-beginning))
                 (end (region-end)))
             (concat (timfel/agent-shell-vc--file-header-before beg)
                     (buffer-substring-no-properties
                      (car (diff-bounds-of-hunk)) end))))
          (t
           (let* ((pos (point))
                  (hunk-bounds (ignore-errors (diff-bounds-of-hunk)))
                  (file-bounds (diff-bounds-of-file)))
             (if (and hunk-bounds (<= (car hunk-bounds) pos))
                 (concat (timfel/agent-shell-vc--file-header-before pos)
                         (timfel/agent-shell-vc--substring hunk-bounds))
               (timfel/agent-shell-vc--substring file-bounds))))))
      (error nil))))

(defun timfel/agent-shell-vc--tracked-files (backend files)
  "Return FILES that BACKEND can diff."
  (seq-filter
   (lambda (file)
     (condition-case nil
         (not (eq (vc-call-backend backend 'state file) 'unregistered))
       (error nil)))
   files))

(defun timfel/agent-shell-vc--vc-dir-files ()
  "Return the marked vc-dir files or the file at point."
  (when (derived-mode-p 'vc-dir-mode)
    (or (vc-dir-marked-files)
        (list (vc-dir-current-file)))))

(defun timfel/agent-shell-vc--vc-dir-patch ()
  "Return a synchronous VC diff for the current `vc-dir' selection."
  (when (derived-mode-p 'vc-dir-mode)
    (when-let* ((backend (or vc-dir-backend (vc-deduce-backend)))
                (files (timfel/agent-shell-vc--tracked-files
                        backend (timfel/agent-shell-vc--vc-dir-files))))
      (when files
        (condition-case nil
            (with-temp-buffer
              (let ((default-directory (or (ignore-errors (vc-root-dir))
                                           default-directory)))
                (vc-call-backend backend 'diff files nil nil (current-buffer) nil)
                (string-trim-right (buffer-string))))
          (error nil))))))

;;;###autoload
(defun timfel/agent-shell-vc-context-source ()
  "Return the current built-in VC diff as `agent-shell' context, or nil."
  (let* ((patch (or (timfel/agent-shell-vc--diff-mode-patch)
                    (timfel/agent-shell-vc--vc-dir-patch)))
         (backend (cond
                   ((derived-mode-p 'diff-mode) diff-vc-backend)
                   ((derived-mode-p 'vc-dir-mode) vc-dir-backend)))
         (revisions (when (derived-mode-p 'diff-mode)
                      diff-vc-revisions))
         (files (cond
                 ((derived-mode-p 'diff-mode)
                  (ignore-errors (cadr (diff-vc-deduce-fileset))))
                 ((derived-mode-p 'vc-dir-mode)
                  (timfel/agent-shell-vc--vc-dir-files)))))
    (when (and patch (not (string-empty-p patch)))
      (timfel/agent-shell-vc--format-context backend revisions files patch))))

(provide 'timfel-agent-shell-vc)

;;; timfel-agent-shell-vc.el ends here
