;;; timfel-agent-shell-magit.el --- Agent-shell context helpers for Magit -*- lexical-binding: t -*-

;;; Commentary:

;; Turn the current Magit diff hunk into `agent-shell' context.

;;; Code:

(require 'subr-x)
(require 'magit-diff)
(require 'magit-section)
(require 'magit-mode)

(defun timfel/agent-shell-magit--hunk-text (section)
  "Return the diff hunk text for SECTION."
  (buffer-substring-no-properties (oref section start) (oref section end)))

(defun timfel/agent-shell-magit--file-context (section)
  "Return full file diff context for SECTION."
  (concat
   (magit-diff-file-header section)
   (mapconcat #'timfel/agent-shell-magit--hunk-text
              (delq nil
                    (mapcar (lambda (child)
                              (when (magit-hunk-section-p child)
                                child))
                            (oref section children)))
              "")))

(defun timfel/agent-shell-magit--region-context (section)
  "Return diff context for SECTION.

Use the active internal hunk region when present, otherwise include the full
hunk or file."
  (cond
   ((magit-file-section-p section)
    (timfel/agent-shell-magit--file-context section))
   ((and (region-active-p)
         (magit-section-internal-region-p section))
    (concat (magit-diff-file-header section)
            (magit-diff-hunk-region-patch section)))
   (t
    (concat (magit-diff-file-header section)
            (timfel/agent-shell-magit--hunk-text section)))))

(defun timfel/agent-shell-magit--selected-section ()
  "Return the current hunk or file section, or nil when none is selected.

If the region is active, it has to stay inside a single hunk body."
  (when-let ((section (magit-current-section)))
    (cond
     ((and (magit-hunk-section-p section)
           (or (not (region-active-p))
               (magit-section-internal-region-p section)))
      section)
     ((and (magit-file-section-p section)
           (not (region-active-p)))
      section))))

(defun timfel/agent-shell-magit--format-context (commit-sha patch)
  "Format Magit diff context for `agent-shell'."
  (string-join
   (delq nil
         (list "[MAGIT DIFF CONTEXT]"
               (when commit-sha
                 (format "Commit-ish: %s" commit-sha))
               ""
               "```diff"
               patch
               "```"
               "[END CONTEXT]"))
   "\n"))

;;;###autoload
(defun timfel/agent-shell-magit-context-source ()
  "Return the current Git diff section as `agent-shell' context, or nil."
  (when-let* ((section (timfel/agent-shell-magit--selected-section))
              (patch (string-trim-right
                      (timfel/agent-shell-magit--region-context section))))
    (unless (string-empty-p patch)
      (timfel/agent-shell-magit--format-context (magit-buffer-revision) patch))))

(provide 'timfel-agent-shell-magit)

;;; timfel-agent-shell-magit.el ends here
