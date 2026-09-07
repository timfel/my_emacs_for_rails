;;; gptel-compile.el --- Compile pseudocode into a project patch -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff <timfelgentreff@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, version 3.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; `gptel-compile' turns a selected pseudocode specification into a unified
;; project patch.  The model can inspect, but not change, files below the
;; current project root.  Its final response is displayed in an editable
;; `diff-mode' buffer, where it can be applied or rejected.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'gptel)
(require 'gptel-request)
(require 'project)
(require 'subr-x)

(defgroup gptel-compile nil
  "Compile pseudocode into project patches with gptel."
  :group 'gptel)

(defcustom gptel-compile-max-toolcalls 20
  "Maximum number of project files a compile request may read."
  :type 'integer
  :group 'gptel-compile)

(defconst gptel-compile--system-prompt
  "You are an expert programmer compiling pseudocode into a complete project change.
You may inspect project files with the read tool before deciding on the change.
The tool is read-only and may be used at most 20 times.  Once its budget is
exhausted, you must reply with the patch; do not request another tool call.
Make every edit required by the pseudocode, including edits to related files.
Your final response MUST be one complete, applicable unified diff and nothing
else: no explanation, markdown fence, or progress report.  Use git-style paths
relative to the project root (--- a/path and +++ b/path), including /dev/null
when adding or deleting a file.  Do not abbreviate hunks.  Ensure every hunk
header has exact old and new line counts, and terminate the diff with a newline."
  "System prompt used by `gptel-compile'.")

(defvar gptel-compile-patch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "C-c C-c") #'gptel-compile-apply)
    (define-key map (kbd "C-c C-k") #'gptel-compile-reject)
    map)
  "Keymap for `gptel-compile-patch-mode'.")

(define-derived-mode gptel-compile-patch-mode diff-mode "GPTel Compile Patch"
  "Mode for an editable patch produced by `gptel-compile'."
  (setq-local buffer-read-only nil)
  (setq-local header-line-format
              (substitute-command-keys
               " Edit the patch, then \\[gptel-compile-apply] apply or \\[gptel-compile-reject] reject")))

(defvar-local gptel-compile--root nil)
(defvar-local gptel-compile--window-configuration nil)
(defvar-local gptel-compile--finished nil)

(defun gptel-compile--read-file (root filename)
  "Read FILENAME below ROOT, returning text or a useful tool error.

ROOT is fixed when the request starts, so changing `default-directory' while a
request is active cannot broaden the tool's access."
  (condition-case err
      (let* ((file (expand-file-name filename root))
             ;; `file-truename' resolves symlinks before the containment check.
             (true-file (and (file-exists-p file) (file-truename file))))
        (cond
         ((not (stringp filename)) "Error: filename must be a string")
         ((not true-file) (format "Error: file does not exist: %s" filename))
         ((not (file-in-directory-p true-file root))
          (format "Error: %s is outside the project root" filename))
         ((not (file-regular-p true-file))
          (format "Error: not a regular file: %s" filename))
         ((not (file-readable-p true-file))
          (format "Error: file is not readable: %s" filename))
         (t
          (with-temp-buffer
            (insert-file-contents true-file)
            (buffer-string)))))
    (error (format "Error reading %s: %s" filename (error-message-string err)))))

(defun gptel-compile--make-read-tool (root)
  "Return the read tool permitted for a request rooted at ROOT."
  (let ((reads 0))
    (gptel-make-tool
     :name "read"
     :function
     (lambda (filename)
       (if (>= reads gptel-compile-max-toolcalls)
           (format "Error: the %d read budget is exhausted. Reply now with the unified diff."
                   gptel-compile-max-toolcalls)
         (cl-incf reads)
         (let ((result (gptel-compile--read-file root filename)))
           (if (= reads gptel-compile-max-toolcalls)
               (concat result "\n\nToolcall budget exhausted. Reply now with the unified diff.")
             result))))
     :description
     "Read a text file in this project. Paths outside the project are rejected."
     :args (list '(:name "filename" :type string :description "Filename to read."))
     :category "compile"
     :confirm nil
     :include nil)))

(defun gptel-compile--grep (cb pattern root)
  "Asynchronously grep for PATTERN below ROOT and call CB with the result.

Use a private process buffer rather than `deadgrep'.  `deadgrep' maintains a
small global pool of result buffers and may prompt while evicting one; that is
not appropriate for a background tool call."
  (let ((default-directory root)
        (output (generate-new-buffer " *gptel-compile-grep*"))
        (executable (executable-find "grep")))
    (if (not executable)
        (progn
          (kill-buffer output)
          (funcall cb "Error: `grep' executable not found"))
      (condition-case err
          (make-process
           :name "gptel-compile-grep"
           :buffer output
           :command (list executable "-r" "-n" "-H" "-F" "-I"
                          "--exclude-dir=.git" "-e" pattern ".")
           :coding 'utf-8-unix
           :noquery t
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (let* ((status (process-exit-status process))
                      (results (if (buffer-live-p output)
                                   (with-current-buffer output
                                     (buffer-string))
                                 "")))
                 (when (buffer-live-p output)
                   (kill-buffer output))
                 (funcall
                  cb
                  (cond
                   ((= status 0) results)
                   ((= status 1) (if (string-empty-p results)
                                       "No matches."
                                     results))
                   (t (format "Error: grep exited with status %d%s"
                              status
                              (if (string-empty-p results)
                                  ""
                                (concat ": " (string-trim results)))))))))))
        (error
         (kill-buffer output)
         (funcall cb (format "Error starting grep: %s"
                             (error-message-string err))))))))

(defun gptel-compile--make-grep-tool (root)
  "Return the grep tool permitted for a request rooted at ROOT."
  (let ((greps 0))
    (gptel-make-tool
     :name "grep"
     :async t
     :function
     (lambda (cb pattern)
       (if (>= greps gptel-compile-max-toolcalls)
           (funcall cb (format "Error: the %d grep budget is exhausted."
                               gptel-compile-max-toolcalls))
         (cl-incf greps)
         (gptel-compile--grep cb pattern root)))
     :description
     "Grep for a pattern in this project."
     :args (list '(:name "pattern" :type string :description "Pattern to grep for"))
     :category "compile"
     :confirm nil
     :include nil)))

;; This matches gptel-rewrite's FSM, but runs this command's read tool without
;; being affected by a user's global tool-confirmation preference.
(defvar gptel-compile--handlers
  `((WAIT ,#'gptel--handle-wait)
    (TPRE ,#'gptel--handle-pre-tool ,#'gptel--fsm-transition)
    (TOOL ,#'gptel-compile--handle-tool-use)
    (TRET ,#'gptel--handle-post-tool ,#'gptel--handle-tool-result))
  "FSM handlers used by `gptel-compile'.")

(defun gptel-compile--handle-tool-use (fsm)
  "Run compile's read-only tools in FSM without confirmation."
  (let ((gptel-confirm-tool-calls nil))
    (gptel--handle-tool-use fsm)))

(defun gptel-compile--source-name (buffer root)
  "Return BUFFER's project-relative file name, or a descriptive fallback."
  (if-let* ((file (buffer-file-name buffer))
            ((file-exists-p file))
            (true-file (file-truename file))
            ((file-in-directory-p true-file root)))
      (file-relative-name true-file root)
    "(not visiting a project file)"))

(defun gptel-compile--request-prompt (pseudocode source root)
  "Build the user prompt from PSEUDOCODE, SOURCE, and ROOT."
  (format (concat "Project root: %s\n"
                  "Selected in: %s\n\n"
                  "Compile the following pseudocode specification into the required "
                  "project change.  Inspect files when useful, then return the unified diff.\n\n"
                  "--- pseudocode ---\n%s\n--- end pseudocode ---")
          root source pseudocode))

(defun gptel-compile--finish (&optional patch-buffer)
  "Dispose of PATCH-BUFFER and restore its saved window configuration."
  (let ((buffer (or patch-buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless gptel-compile--finished
          (setq gptel-compile--finished t)
          (let ((configuration gptel-compile--window-configuration))
            (when (window-configuration-p configuration)
              (set-window-configuration configuration))))))))

(defun gptel-compile--revert-project-buffers (root)
  "Refresh unmodified visiting buffers below ROOT after a patch is applied."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((file buffer-file-name)
                  ((not (buffer-modified-p)))
                  ((file-exists-p file))
                  (true-file (file-truename file))
                  ((file-in-directory-p true-file root)))
        (revert-buffer t t)))))

(defun gptel-compile--call-patch (patch root &rest arguments)
  "Run git apply on PATCH below ROOT with ARGUMENTS.
Return a cons of its exit status and complete output.

`--recount' is used by the caller because LLM-generated hunks sometimes have
incorrect line counts."
  (let ((output (generate-new-buffer " *gptel-compile-patch-output*"))
        (default-directory root))
    (unwind-protect
        (with-temp-buffer
          (insert patch)
          ;; `git apply' rejects a patch whose final line is not terminated.
          (unless (bolp)
            (insert "\n"))
          (cons (apply #'call-process-region
                       (point-min) (point-max) "git" nil output nil arguments)
                (with-current-buffer output (buffer-string))))
      (kill-buffer output))))

;;;###autoload
(defun gptel-compile-apply ()
  "Apply the editable patch in the current `gptel-compile' patch buffer."
  (interactive)
  (unless (derived-mode-p 'gptel-compile-patch-mode)
    (user-error "This is not a gptel-compile patch buffer"))
  (unless (executable-find "git")
    (user-error "Cannot apply patch: `git' executable not found"))
  (let* ((patch (buffer-substring-no-properties (point-min) (point-max)))
         (root gptel-compile--root)
         (dry-run (gptel-compile--call-patch patch root "apply" "--check" "--recount")))
    (if (not (zerop (car dry-run)))
        (message "Patch does not apply: %s"
                 (string-trim (truncate-string-to-width (cdr dry-run) 300 nil nil "…")))
      (let ((result (gptel-compile--call-patch patch root "apply" "--recount")))
        (if (not (zerop (car result)))
            (message "Patch failed: %s"
                     (string-trim (truncate-string-to-width (cdr result) 300 nil nil "…")))
          (gptel-compile--revert-project-buffers root)
          (let ((buffer (current-buffer)))
            (gptel-compile--finish buffer)
            (kill-buffer buffer))
          (message "Applied gptel-compile patch."))))))

;;;###autoload
(defun gptel-compile-reject ()
  "Reject the editable patch in the current `gptel-compile' patch buffer."
  (interactive)
  (unless (derived-mode-p 'gptel-compile-patch-mode)
    (user-error "This is not a gptel-compile patch buffer"))
  (let ((buffer (current-buffer)))
    (gptel-compile--finish buffer)
    (kill-buffer buffer))
  (message "Rejected gptel-compile patch."))

(defun gptel-compile--show-patch (patch context)
  "Show PATCH in an editable diff buffer using request CONTEXT."
  (let ((buffer (generate-new-buffer "*gptel-compile patch*")))
    (with-current-buffer buffer
      (insert patch)
      (unless (bolp)
        (insert "\n"))
      (goto-char (point-min))
      (gptel-compile-patch-mode)
      (setq-local gptel-compile--root (plist-get context :root)
                  gptel-compile--window-configuration
                  (plist-get context :window-configuration))
      (add-hook 'kill-buffer-hook #'gptel-compile--finish nil t))
    (pop-to-buffer buffer)))

(defun gptel-compile--stream-append (context chunk)
  "Append streamed response CHUNK to the mutable state in CONTEXT."
  (when-let* ((state (plist-get context :stream-state)))
    ;; Keep chunks separate while they arrive.  Concatenating each chunk to a
    ;; growing string makes long patches unnecessarily expensive.
    (setcar state (cons chunk (car state)))))

(defun gptel-compile--stream-finish (context)
  "Return and reset the accumulated streamed response in CONTEXT."
  (when-let* ((state (plist-get context :stream-state)))
    (prog1 (mapconcat #'identity (nreverse (car state)) "")
      (setcar state nil))))

(defun gptel-compile--callback (response info)
  "Handle a compile RESPONSE using request INFO.

When streaming is enabled, gptel calls this function once for every text
chunk and once more with `t' when the response is complete.  Tool-call
responses also end with `t', so only show the accumulated text when the
current response did not contain a tool call."
  (let ((context (plist-get info :context)))
    (cond
     ((stringp response)
      (if (plist-get info :stream)
          (gptel-compile--stream-append context response)
        ;; Backends that do not support streaming return the complete response
        ;; as one string and omit :stream from INFO.
        (gptel-compile--show-patch response context)))
     ((eq response t)
      (if (plist-get info :tool-use)
          ;; Discard any text emitted before a tool call.  The tool result
          ;; callback will cause the next request to start with fresh state.
          (gptel-compile--stream-finish context)
        (gptel-compile--show-patch
         (gptel-compile--stream-finish context) context)))
     ((or (eq response 'abort) (null response))
      (gptel-compile--stream-finish context)
      (message "gptel-compile failed: %s" (or (plist-get info :status) "request aborted")))
     ((eq (car-safe response) 'tool-result)
      ;; A tool-result starts a new streamed response.  In particular, do not
      ;; let a pre-tool textual fragment become part of the final patch.
      (gptel-compile--stream-finish context))
     ((eq (car-safe response) 'tool-call)
      ;; The dedicated FSM does not normally expose calls for confirmation, but
      ;; accepting here keeps the request moving if another gptel extension does.
      (dolist (call (cdr response))
        (pcase-let ((`(,tool ,args ,continue) call))
          (funcall continue
                   (apply (gptel-tool-function tool)
                          (gptel--map-tool-args tool args))))))
     ;; Optional reasoning is not user-facing for this compact workflow.
     (t nil))))

;;;###autoload
(defun gptel-compile (begin end)
  "Compile pseudocode in region BEGIN through END into an editable project patch."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (user-error "Select pseudocode to compile")))
  (unless (< begin end)
    (user-error "Select non-empty pseudocode to compile"))
  (let* ((buffer (current-buffer))
         (root (project-root (project-current)))
         (pseudocode (buffer-substring-no-properties begin end))
         (source (gptel-compile--source-name buffer root))
         ;; The state is a one-element list so asynchronous stream callbacks
         ;; can update it without relying on rebinding the request context.
         (context (list :root root
                        :stream-state (list nil)
                        :window-configuration (current-window-configuration)))
         ;; Keep this deliberately isolated from chat context and any configured
         ;; agent tools: a compile request gets precisely its one read tool.
         (gptel-use-context nil)
         (gptel-use-tools t)
         (gptel-tools (list
                       (gptel-compile--make-read-tool root)
                       (gptel-compile--make-grep-tool root)))
         (gptel-include-reasoning nil)
         (gptel-temperature 0.0)
         (gptel-stream t))
    (gptel-request
        (gptel-compile--request-prompt pseudocode source root)
      :buffer buffer
      :stream t
      :system gptel-compile--system-prompt
      :context context
      :fsm (gptel-make-fsm :handlers gptel-compile--handlers)
      :callback #'gptel-compile--callback)
    (setq deactivate-mark t)))

(provide 'gptel-compile)
;;; gptel-compile.el ends here
