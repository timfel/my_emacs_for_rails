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
(require 'org)
(require 'project)
(require 'cl-lib)
(require 'subr-x)

(defvar-local gptel-pi-session-p nil
  "Non-nil when the current buffer is a gptel-pi session.")

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

(defconst gptel-pi--protected-org-blocks
  '("src" "example" "quote" "reasoning" "tool")
  "Org block names in which assistant headings remain untouched.")

(defun gptel-pi--response-properties (position)
  "Return role-related text properties copied from POSITION."
  (let ((gptel (get-text-property position 'gptel)))
    (when gptel
      (list 'gptel gptel 'front-sticky '(gptel)))))

(defun gptel-pi--normalize-heading-at-point (begin)
  "Turn the Org heading at point into a bold label when it starts after BEGIN."
  (when (looking-at "^\\(\\*+\\)[ \t]+\\(.+?\\)[ \t]*$")
    (let* ((heading-start (match-beginning 1))
           (title-start (match-beginning 2))
           (title-end (match-end 2))
           (properties (gptel-pi--response-properties title-start)))
      (when (>= heading-start begin)
        (goto-char title-end)
        (insert (apply #'propertize "*" properties))
        (delete-region heading-start title-start)
        (goto-char heading-start)
        (insert (apply #'propertize "*" properties))))))

(defun gptel-pi-normalize-assistant-headings (begin end)
  "Turn assistant Org headings between BEGIN and END into bold labels.

Headings in source, example, quote, reasoning, and tool blocks are not changed."
  (when (and gptel-pi-session-p
             (derived-mode-p 'org-mode)
             (< begin end))
    (let ((end-marker (copy-marker end t))
          (case-fold-search t)
          block-stack)
      (unwind-protect
          (save-excursion
            (save-restriction
              (widen)
              (goto-char begin)
              (unless (bolp) (forward-line 1))
              (while (< (point) end-marker)
                (let ((next-line
                       (copy-marker
                        (save-excursion (forward-line 1) (point)) t)))
                  (cond
                   ((looking-at "^[ \t]*#\\+begin_\\([[:alnum:]_-]+\\)\\b")
                    (let ((name (downcase (match-string-no-properties 1))))
                      (when (member name gptel-pi--protected-org-blocks)
                        (push name block-stack))))
                   ((looking-at "^[ \t]*#\\+end_\\([[:alnum:]_-]+\\)\\b")
                    (let ((name (downcase (match-string-no-properties 1))))
                      (when (equal name (car block-stack))
                        (pop block-stack))))
                   ((null block-stack)
                    (gptel-pi--normalize-heading-at-point begin)))
                  (goto-char next-line)
                  (set-marker next-line nil)))))
        (set-marker end-marker nil)))))

(defun gptel-pi-promote-label (&optional level)
  "Promote the bold assistant label at point to an Org heading.

Use heading LEVEL when supplied.  Interactively, a numeric prefix specifies the
level; otherwise use one level below the containing heading."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "This command is only available in Org buffers"))
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^\\*\\([^*\n]+\\)\\*[ \t]*$")
      (user-error "Current line is not a bold assistant label"))
    (let* ((title-start (match-beginning 1))
           (title-end (match-end 1))
           (title (buffer-substring title-start title-end))
           (properties (gptel-pi--response-properties title-start))
           (heading-level (if level
                              (prefix-numeric-value level)
                            (1+ (or (org-current-level) 0)))))
      (unless (> heading-level 0)
        (user-error "Heading level must be positive"))
      (delete-region (line-beginning-position) (line-end-position))
      (insert (apply #'propertize
                     (concat (make-string heading-level ?*) " " title)
                     properties)))))

(defun gptel-pi--initialize-org-session ()
  "Give a new Org session explicit archive and conversation subtrees."
  (when (derived-mode-p 'org-mode)
    (let ((initial (buffer-substring (point-min) (point-max))))
      (erase-buffer)
      (insert "* Archive\n** Tool outputs\n\n** Compactions\n\n"
              "* Conversation\n"
              initial)
      (goto-char (point-max)))))

(defun gptel-pi--next-target (kind)
  "Return a unique, monotonically increasing target for KIND."
  (let ((regexp (format "<<gptel-pi-%s-\\([0-9]+\\)>>" (regexp-quote kind)))
        (maximum 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq maximum (max maximum (string-to-number (match-string 1))))))
    (format "gptel-pi-%s-%04d" kind (1+ maximum))))

(defun gptel-pi--archive-section-position (section)
  "Return insertion position at the end of Archive subsection SECTION."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Archive[ \t]*$" nil t)
      (error "This gptel-pi buffer has no Archive heading"))
    (let ((archive-end (save-excursion (org-end-of-subtree t) (point))))
      (unless (re-search-forward
               (format "^\\*\\* %s[ \t]*$" (regexp-quote section))
               archive-end t)
        (error "This gptel-pi buffer has no Archive/%s section" section))
      (org-end-of-subtree t)
      (point))))

(defun gptel-pi--archive-entry (kind section title body &optional metadata)
  "Archive BODY under SECTION and return a link to its KIND target.

TITLE is used for the level-three heading.  Optional METADATA is ordinary Org
text inserted before the example block."
  (unless (and gptel-pi-session-p (derived-mode-p 'org-mode))
    (error "Tool output can only be archived in an Org gptel-pi session"))
  (let ((target (gptel-pi--next-target kind))
        (body (gptel-pi--result-string body)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (gptel-pi--archive-section-position section))
        (unless (bolp) (insert "\n"))
        (insert (format "*** %s\n<<%s>>\n\n" title target))
        (when metadata
          (insert metadata)
          (unless (string-suffix-p "\n" metadata) (insert "\n"))
          (insert "\n"))
        (insert "#+begin_example\n"
                (org-escape-code-in-string body))
        (unless (string-suffix-p "\n" body) (insert "\n"))
        (insert "#+end_example\n\n")))
    (format "[[%s][%s]]" target title)))

(defun gptel-pi--skill-summary (path root)
  "Return a short skill index line for PATH relative to ROOT."
  (with-temp-buffer
    (insert-file-contents path nil 0 2000)
    (let ((case-fold-search nil)
          name description)
      (goto-char (point-min))
      (when (re-search-forward "^name:[ \t]*\\(.+\\)$" nil t)
        (setq name (string-trim (match-string 1))))
      (goto-char (point-min))
      (when (re-search-forward "^description:[ \t]*\\(.+\\)$" nil t)
        (setq description (string-trim (match-string 1))))
      (format "- %s (%s): %s"
              (or name (file-name-nondirectory (directory-file-name
                                                (file-name-directory path))))
              (file-relative-name path root)
              (or description "No description provided")))))

(defun gptel-pi--project-system-context (root)
  "Build branch-invariant project instructions and skill index for ROOT."
  (let* ((agents (expand-file-name "AGENTS.md" root))
         (agents (and (file-readable-p agents) agents))
         (skill-dirs (list (expand-file-name ".agents/skills/" root)
                           (expand-file-name "~/.agents/skills/")))
         (skills (cl-loop for dir in skill-dirs
                          when (file-directory-p dir)
                          append (directory-files-recursively dir "SKILL\\.md\\'")))
         (extra-agents
          (cl-remove-if
           (lambda (path) (and agents (file-equal-p path agents)))
           (directory-files-recursively root "AGENTS\\.md\\'" nil
                                        (lambda (dir)
                                          (< (length (file-name-split
                                                      (file-relative-name dir root)))
                                             4))))))
    (when (or agents skills extra-agents)
      (concat
       "\n\nProject context (applies to every conversation branch):\n"
       (when agents
         (concat "\n<project_instructions path=\"" agents "\">\n"
                 (with-temp-buffer
                   (insert-file-contents agents)
                   (buffer-string))
                 "\n</project_instructions>\n"))
       (when skills
         (concat "\nAvailable skills (read the referenced file when relevant):\n"
                 (mapconcat (lambda (path) (gptel-pi--skill-summary path root))
                            skills "\n")
                 "\n"))
       (when extra-agents
         (concat "\nSubdirectory-specific instructions (read when working below them):\n"
                 (mapconcat (lambda (path)
                              (concat "- " (file-relative-name path root)))
                            extra-agents "\n")
                 "\n"))))))

(defun gptel-pi--setup-buffer ()
  "Set up the current buffer as a gptel-pi session."
  (setq-local gptel-pi-session-p t)
  (add-hook 'gptel-post-response-functions
            #'gptel-pi-normalize-assistant-headings nil t))

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

With a C-u C-u prefix, ask for the *gptel-pi* buffer to switch to."
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
                           (bound-and-true-p gptel-pi-session-p)
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
        (gptel-pi--setup-buffer)
        (when-let* ((project-context (gptel-pi--project-system-context root)))
          (setq-local gptel-system-prompt
                      (concat gptel-system-prompt project-context)))
        (gptel-pi--initialize-org-session)))

    (pop-to-buffer buffer)))

(provide 'gptel-pi)
