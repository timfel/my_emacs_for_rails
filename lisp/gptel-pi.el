;;; gptel-pi.el --- GPTel configuration to be a minimal agent. Like Pi :) -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff <timfelgentreff@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, version 3.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see https://www.gnu.org/licenses/.

;;; Author: Tim Felgentreff <timfelgentreff@gmail.com>

;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'project)
(require 'cl-lib)
(require 'subr-x)

(defconst gptel-pi-system-prompt "You are an expert coding assistant.
You help users with coding tasks by reading files, executing commands, editing code, and writing new files.

Guidelines:
- Use `eval` to get recent buffers, kill ring, or other editor state
- Use `bash` for file operations like ls, grep, find
- Use `read` to examine files before editing
- Use `edit` for precise changes
- Use `write` only for new files or complete rewrites
- When summarizing your actions, output plain text directly - do NOT use cat or bash to display what you did
- Be concise in your responses")

(defun gptel-pi--sandboxed-p (buffer)
  "Return non-nil when BUFFER has an effective shell command prefix."
  (when (and (buffer-live-p buffer)
             (boundp 'agent-shell-command-prefix))
    (let ((prefix (buffer-local-value 'agent-shell-command-prefix buffer)))
      (if (functionp prefix)
          (with-current-buffer buffer
            (funcall prefix buffer))
        prefix))))

(defun gptel-pi--restrict-tools (fsm)
  "Remove Emacs-host tools from the request when shell access is sandboxed.

The shell command prefix only affects processes started by the `bash' tool;
`read', `write', `edit', and `eval' execute in Emacs itself.  Do not advertise
those tools in a sandboxed request."
  (let ((buffer (plist-get (gptel-fsm-info fsm) :buffer)))
    (when (gptel-pi--sandboxed-p buffer)
      (setq gptel-tools
            (cl-remove-if
             (lambda (tool)
               (member (gptel-tool-name tool) '("eval" "read" "write" "edit")))
             gptel-tools)))))

(defcustom gptel-pi-max-tool-bytes (* 50 1024)
  "Maximum UTF-8 bytes returned directly by a gptel-pi tool."
  :type 'integer
  :group 'gptel)

(defcustom gptel-pi-max-tool-lines 2000
  "Maximum complete lines returned directly by a gptel-pi tool."
  :type 'integer
  :group 'gptel)

(defun gptel-pi--result-string (result)
  "Convert arbitrary tool RESULT to readable text."
  (if (stringp result)
      result
    (condition-case nil
        (prin1-to-string result)
      (error (format "%s" result)))))

(defun gptel-pi--truncate-result (result direction &optional max-bytes max-lines)
  "Truncate RESULT by complete lines and UTF-8 bytes.

DIRECTION is either `head' or `tail'.  MAX-BYTES and MAX-LINES default to
`gptel-pi-max-tool-bytes' and `gptel-pi-max-tool-lines'.  Return a plist with
`:content', `:truncated', `:lines', `:bytes', and `:first-line-too-long'."
  (let* ((text (gptel-pi--result-string result))
         (max-bytes (or max-bytes gptel-pi-max-tool-bytes))
         (max-lines (or max-lines gptel-pi-max-tool-lines))
         (total-bytes (string-bytes text))
         segments)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((start (point)))
          (forward-line 1)
          (push (buffer-substring-no-properties start (point)) segments))))
    (setq segments (nreverse segments))
    (if (and (<= total-bytes max-bytes)
             (<= (length segments) max-lines))
        (list :content text :truncated nil :lines (length segments)
              :bytes total-bytes :first-line-too-long nil)
      (let ((candidates (if (eq direction 'tail) (reverse segments) segments))
            (kept nil)
            (bytes 0)
            (lines 0))
        (while (and candidates (< lines max-lines))
          (let* ((segment (car candidates))
                 (segment-bytes (string-bytes segment)))
            (if (> (+ bytes segment-bytes) max-bytes)
                (setq candidates nil)
              (push segment kept)
              (setq bytes (+ bytes segment-bytes)
                    lines (1+ lines)
                    candidates (cdr candidates)))))
        (when (eq direction 'head)
          (setq kept (nreverse kept)))
        (list :content (apply #'concat kept)
              :truncated t
              :lines lines
              :bytes bytes
              :first-line-too-long (and (eq direction 'head) (zerop lines)
                                         (not (string-empty-p text))))))))

(defun gptel-pi--file-line-count ()
  "Return the number of text lines in the current buffer."
  (if (= (point-min) (point-max))
      0
    (line-number-at-pos (max (point-min) (1- (point-max))))))

(defun gptel-pi--tool-read (path &optional offset limit)
  "Read text file PATH from one-indexed OFFSET for at most LIMIT lines."
  (let ((expanded-path (expand-file-name path))
        (offset (or offset 1)))
    (cond
     ((or (not (integerp offset)) (< offset 1))
      (format "Error: offset must be a positive integer, got %S" offset))
     ((and limit (or (not (integerp limit)) (< limit 1)))
      (format "Error: limit must be a positive integer, got %S" limit))
     ((not (file-readable-p expanded-path))
      (format "Error: file is not readable: %s" expanded-path))
     ((file-directory-p expanded-path)
      (format "Error: path is a directory: %s" expanded-path))
     (t
      (with-temp-buffer
        (insert-file-contents expanded-path)
        (let ((total-lines (gptel-pi--file-line-count)))
          (cond
           ((zerop total-lines)
            (format "File is empty: %s" expanded-path))
           ((> offset total-lines)
            (format "Error: offset %d is beyond end of file (%d lines total): %s"
                    offset total-lines expanded-path))
           (t
            (goto-char (point-min))
            (forward-line (1- offset))
            (let ((start (point)))
              (if limit
                  (forward-line limit)
                (goto-char (point-max)))
              (let* ((selected (buffer-substring-no-properties start (point)))
                     (truncation (gptel-pi--truncate-result selected 'head))
                     (shown-lines (plist-get truncation :lines)))
                (if (plist-get truncation :first-line-too-long)
                    (format (concat "Line %d exceeds the %d-byte read limit. "
                                    "Inspect it in chunks, for example with: "
                                    "sed -n '%dp' %s | head -c %d")
                            offset gptel-pi-max-tool-bytes offset
                            (shell-quote-argument expanded-path)
                            gptel-pi-max-tool-bytes)
                  (let* ((end-line (+ offset shown-lines -1))
                         (next-offset (and (< end-line total-lines)
                                           (1+ end-line))))
                    (concat
                     (plist-get truncation :content)
                     (unless (or (string-empty-p (plist-get truncation :content))
                                 (string-suffix-p "\n" (plist-get truncation :content)))
                       "\n")
                     (format "[Showing lines %d-%d of %d%s.]"
                             offset end-line total-lines
                             (if next-offset
                                 (format "; use offset=%d to continue" next-offset)
                               "")))))))))))))))

(defun gptel-pi--tool-eval (elisp)
  "Evaluate one ELISP form and return its printed result."
  (plist-get (gptel-pi--truncate-result (eval (read elisp)) 'head) :content))

(defun gptel-pi--tool-edit (path old new)
  "Replace the unique literal occurrence of OLD with NEW in PATH."
  (let ((expanded-path (expand-file-name path)))
    (cond
     ((string-empty-p old)
      "Error: old text must not be empty")
     ((not (file-exists-p expanded-path))
      (format "Error: file not found: %s" expanded-path))
     ((file-directory-p expanded-path)
      (format "Error: path is a directory: %s" expanded-path))
     (t
      (with-temp-buffer
        (insert-file-contents expanded-path)
        (let ((matches 0)
              first-start)
          (goto-char (point-min))
          (while (search-forward old nil t)
            (setq matches (1+ matches))
            (unless first-start
              (setq first-start (match-beginning 0)))
            ;; Advance one character from the beginning to count overlapping
            ;; literal matches as ambiguous too.
            (goto-char (1+ (match-beginning 0))))
          (cond
           ((zerop matches)
            (format "Error: old text not found in %s" expanded-path))
           ((> matches 1)
            (format "Error: old text matches %d times in %s; it must be unique"
                    matches expanded-path))
           (t
            (goto-char first-start)
            (let ((line (line-number-at-pos)))
              (delete-region first-start (+ first-start (length old)))
              (insert new)
              (write-region (point-min) (point-max) expanded-path nil 'silent)
              (format "Edited %s at line %d" expanded-path line))))))))))

(defun gptel-pi--tool-bash (callback command &optional timeout)
  (let* ((buffer
          (generate-new-buffer
           (format "*%s bash-output*" (string-replace "*" "" (buffer-name)))))
         (effective-timeout
          (let ((seconds (or timeout 300)))
            (when (> seconds 0)
              seconds)))
         (timeout-argv
          (when effective-timeout
            (list "timeout" (format "%ss" effective-timeout))))
         (prefix
          (when (boundp 'agent-shell-command-prefix)
            (cond
             ((functionp agent-shell-command-prefix)
              (funcall agent-shell-command-prefix (current-buffer)))
             (t agent-shell-command-prefix))))
         (argv (append prefix timeout-argv
                       (list shell-file-name shell-command-switch command)))
         (process (make-process :name "bash"
                                :buffer buffer
                                :command argv
                                :connection-type 'pipe
                                :file-handler t
                                :noquery t)))
    (process-put process 'callback callback)
    (set-process-sentinel
     process
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (funcall
          (process-get process 'callback)
          (plist-get
           (gptel-pi--truncate-result
            (format "%s\n%s %d\n"
                    (with-current-buffer (process-buffer process)
                      (buffer-string))
                    (process-status process)
                    (process-exit-status process))
            'tail)
           :content))
         (kill-buffer (process-buffer process)))))
    (process-send-eof process)
    process))

(defconst gptel-pi-tools
  (list
   (gptel-make-tool
    :name "eval"
    :function #'gptel-pi--tool-eval
    :description (concat "Evaluate one Elisp form in the live Emacs session and return its printed value. "
                         "Use this only to inspect relevant editor state such as buffers, the kill ring, "
                         "or active modes; use read and bash for ordinary file and project exploration.")
    :args (list '(:name "elisp" :type string :description "The elisp code to eval"))
    :async nil
    :category "pi"
    :confirm nil
    :include t)

   (gptel-make-tool
    :name "bash"
    :function #'gptel-pi--tool-bash
    :description "Execute a bash command in the current working directory. Returns stdout, stderr, and exit status."
    :args (list '(:name "command" :type string :description "The commandline to execute in bash.")
                '(:name "timeout" :type integer :description "Optional timeout in seconds. Defaults to 300; set to -1 or 0 to disable timeout for this command." :optional t))
    :async t
    :category "pi"
    :confirm nil
    :include t)

   (gptel-make-tool
    :name "read"
    :function #'gptel-pi--tool-read
    :description (concat "Read the contents of a text file. Output is limited to complete lines and "
                         "50KB or 2000 lines, whichever is reached first. Use offset/limit for large "
                         "files and continue with the reported next offset.")
    :args (list '(:name "path" :type string :description "Path to the file to read (relative or absolute)")
                '(:name "offset" :type integer :description "Line number to start reading from (1-indexed)" :optional t)
                '(:name "limit" :type integer :description "Maximum number of lines to read" :optional t))
    :async nil
    :category "pi"
    :confirm nil
    :include t)

   (gptel-make-tool
    :name "write"
    :function (lambda (path content)
                (let ((expanded-path (expand-file-name path)))
                  (make-directory (file-name-directory expanded-path) t)
                  (with-temp-file expanded-path
                    (insert content))
                  (format "Wrote %d bytes to %s"
                          (string-bytes content)
                          expanded-path)))
    :description (concat "Write content to a file. Creates the file if it doesn't exist, overwrites if it does. "
                         "Automatically creates parent directories.")
    :args (list '(:name "path" :type string :description "Path to the file to write (relative or absolute)")
                '(:name "content" :type string :description "Content to write to the file"))
    :async nil
    :category "pi"
    :confirm nil
    :include t)

   (gptel-make-tool
    :name "edit"
    :function #'gptel-pi--tool-edit
    :description (concat "Edit a file by replacing exact text. `old` must match exactly "
                         "(including whitespace). Use this for precise, surgical edits.")
    :args (list '(:name "path" :type string :description "Path to the file to edit (relative or absolute)")
                '(:name "old" :type string :description "Exact text to find and replace (must match exactly)")
                '(:name "new" :type string :description "New text to replace the old text with"))
    :async nil
    :category "pi"
    :confirm nil
    :include t)))

(gptel-make-preset
 'gptel-pi
 :description "gptel pi"
 :system gptel-pi-system-prompt
 :tools (mapcar #'gptel-tool-name gptel-pi-tools)
 :prompt-transform-functions (append
                              gptel-prompt-transform-functions
                              '(gptel-pi--restrict-tools))
 :confirm-tool-calls 'auto
 :use-tools t)

;;;###autoload
(defun gptel-pi (&optional prefix)
  "DWIM open or switch to gptel-mode buffer through the (gptel) command.

Jump to the last visited *gptel-pi* buffer in the current project root,
creating one if none exists.

With a single C-u prefix, start a new *gptel-pi* buffer in the current
project root.

With a C-u C-u prefix, ask for the *gptel-pi* buffer to switch to.

It consults agent-shell-context-sources if that is bound. If it is, it
goes through the sources and the first one that is a callable function
that returns a non-empty string result is used inserted into the new or
existing buffer at (point-max)"
  (interactive "P")
  (let* ((root (file-name-as-directory (file-truename
                (or (when-let ((project (project-current)))
                      (project-root project))
                    default-directory))))
         (buffers
          (let (result)
            (dolist (buffer (buffer-list) (nreverse result))
              (with-current-buffer buffer
                (when (and (bound-and-true-p gptel-mode)
                           (equal (file-truename default-directory) root))
                  (push buffer result))))))
         (buffer
          (cond
           ((equal prefix '(4)) nil)
           ((equal prefix '(16))
            (when buffers
              (get-buffer
               (completing-read "Switch to gptel-pi buffer: "
                                (mapcar #'buffer-name buffers)
                                nil t nil nil 42))))
           (t (car buffers)))))

    ;; new buffer
    (unless buffer
      (setq buffer
            (let ((name (generate-new-buffer-name
                         (format "*gptel-pi:%s*" (abbreviate-file-name root))))
                  (default-directory root))
              (gptel name)))

      (with-current-buffer buffer
        (gptel-preset (gptel-get-preset 'gptel-pi))

        ;; insert agents.md and skills, if any
        (let* ((p (point))
               (heading (if (eq gptel-default-mode 'org-mode) "*" "#"))
               (begin-quote (if (eq gptel-default-mode 'org-mode) "\n#+begin_quote %s\n" "\n``` %s\n"))
               (end-quote
                (lambda ()
                  (if (eq gptel-default-mode 'org-mode)
                      (insert "#+end_quote\n")
                    (insert "```\n"))
                  (let ((p (point)))
                    (forward-line -1)
                    (if (eq gptel-default-mode 'org-mode)
                        (org-cycle)
                      (gptel-markdown-cycle-block))
                    (goto-char p))))
               (amd (expand-file-name "AGENTS.md" root))
               (amd (if (file-readable-p amd) amd nil))
               (skills (thread-last
                         '(".agents/skills/" "~/.agents/skills/")
                         (seq-map (lambda (elt) (expand-file-name elt root)))
                         (seq-filter #'file-directory-p)
                         (seq-mapcat (lambda (elt) (directory-files elt t)))
                         (seq-filter #'file-directory-p)
                         (seq-map (lambda (elt) (file-name-concat elt "SKILL.md")))
                         (seq-filter #'file-exists-p)))
               (extra-amds
                (thread-last
                  (directory-files-recursively root "AGENTS\\.md" nil
                                               (lambda (dir)
                                                 (< (length (file-name-split
                                                             (file-relative-name dir root)))
                                                    4)))
                  (seq-filter (lambda (elt) (not (string-equal elt amd)))))))

          (when (or amd skills extra-amds)
            (goto-char (point-min))
            (insert (format "%s Project instructions:\n" heading))

            (when amd
              (insert (format begin-quote "AGENTS.md"))
              (let ((start (point))
                    (end (cadr (insert-file-contents amd))))
                (forward-char end)
                (indent-rigidly start end 1))
              (funcall end-quote))

            (when skills
              (insert (format begin-quote "Skills (read when it might be useful for the user request)"))
              (dolist (skill skills)
                (insert " - " (file-relative-name skill root) " : ")
                ;; too long skills or incorrectly formatted ones are cut
                (let ((end (+ (point) (cadr (insert-file-contents skill nil 0 500)))))
                  (when (re-search-forward
                         (rx (or (seq "---" (* (or space "\n"))
                                      "name: " (group (* not-newline)) (* (or space "\n"))
                                      "description:" (group (* not-newline)) (* anything))
                                 (* anything)))
                         end t)
                    (replace-match "\\1: \\2\n"))))
              (funcall end-quote))

            (when extra-amds
              (insert (format begin-quote "Subdirectory-specific instructions (read when operating underneath)"))
              (dolist (extra-amd extra-amds)
                (insert " - " (file-relative-name extra-amd root) "\n"))
              (funcall end-quote))

            (insert "\n")
            (forward-char (1- p))))))

    (pop-to-buffer buffer)))

(provide 'gptel-pi)
