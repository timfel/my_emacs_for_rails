;;; timfel-agent-shell-extensions.el --- Agent-shell helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Local helpers for orchestrating `agent-shell' worktrees and shell buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'timfel)
(require 'agent-shell)

(defcustom timfel/agent-shell-worktree-subdirectory
  ".agent-shell/worktrees"
  "Relative directory under a git repository for agent-shell worktrees."
  :type 'string
  :group 'timfel)

(defcustom timfel/agent-shell-planning-request
  "Go into planning mode"
  "Initial request queued before each fan-out agent task."
  :type 'string
  :group 'timfel)

(defun timfel/agent-shell--git-root (&optional directory)
  "Return the git root for DIRECTORY, or nil when outside Git."
  (let ((default-directory
         (file-name-as-directory
          (expand-file-name (or directory default-directory)))))
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil
                                 "rev-parse" "--show-toplevel"))
        (string-trim (buffer-string))))))

(defun timfel/agent-shell--slugify (text)
  "Convert TEXT into a short filesystem-safe slug."
  (let* ((slug (downcase text))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
         (slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug)))
    (if (string-empty-p slug)
        "task"
      (substring slug 0 (min 24 (length slug))))))

(defun timfel/agent-shell--repo-name (repo-root)
  "Return the directory name for REPO-ROOT."
  (file-name-nondirectory
   (directory-file-name (expand-file-name repo-root))))

(defun timfel/agent-shell--worktree-name (title _index)
  "Create the base worktree directory name for TITLE."
  (timfel/agent-shell--slugify title))

(defun timfel/agent-shell--worktree-parent (repo-root title index)
  "Return the per-task parent directory for REPO-ROOT, TITLE, and INDEX.

Use the slugified TITLE by default, appending a numeric suffix only when the
resulting directory already exists.  INDEX is used as the initial fallback
suffix when disambiguation is needed."
  (let* ((base-dir (expand-file-name timfel/agent-shell-worktree-subdirectory
                                     repo-root))
         (base-name (timfel/agent-shell--worktree-name title index))
         (candidate (expand-file-name base-name base-dir))
         (suffix (max 2 index)))
    (while (file-exists-p candidate)
      (setq candidate (expand-file-name (format "%s-%02d" base-name suffix)
                                        base-dir)
            suffix (1+ suffix)))
    candidate))

(defun timfel/agent-shell--mx-linked-sibling-repos (repo-root)
  "Return extra repo roots that should get sibling worktrees with REPO-ROOT.

If REPO-ROOT looks like an mx suite checkout by containing `mx.<repo-name>',
include sibling checkouts named `graal' and `graal-enterprise' when present."
  (let* ((repo-root (expand-file-name repo-root))
         (repo-name (timfel/agent-shell--repo-name repo-root))
         (mx-dir (expand-file-name (format "mx.%s" repo-name) repo-root)))
    (when (file-directory-p mx-dir)
      (seq-filter
       (lambda (sibling)
         (and (file-directory-p sibling)
              (not (file-equal-p repo-root sibling))))
       (mapcar (lambda (name)
                 (expand-file-name (concat "../" name) repo-root))
               '("graal" "graal-enterprise"))))))

(defun timfel/agent-shell--create-single-worktree (repo-root worktree-parent branch)
  "Create one Git worktree for REPO-ROOT below WORKTREE-PARENT on BRANCH."
  (let* ((repo-name (timfel/agent-shell--repo-name repo-root))
         (worktree-dir (expand-file-name repo-name worktree-parent)))
    (when (file-exists-p worktree-dir)
      (user-error "Worktree directory already exists: %s" worktree-dir))
    (make-directory worktree-parent t)
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory repo-root)))
        (unless (zerop (process-file "git" nil t nil
                                     "worktree" "add"
                                     "-b" branch
                                     worktree-dir
                                     "HEAD"))
          (user-error "Failed to create worktree %s: %s"
                      worktree-dir
                      (string-trim (buffer-string))))))
    worktree-dir))

(defun timfel/agent-shell--create-worktree (repo-root title index)
  "Create a Git worktree below REPO-ROOT for TITLE at INDEX.

The primary worktree lives at:
  <repo-root>/<subdir>/<title-slug>/<repo-name>

If REPO-ROOT is an mx suite checkout with an `mx.<repo-name>' directory and a
sibling `graal' or `graal-enterprise' checkout exists, create sibling worktrees
under the same task parent directory as well.  Return the primary worktree
directory."
  (let* ((worktree-parent (timfel/agent-shell--worktree-parent repo-root title index))
         (branch (format "agent-shell/%s"
                         (file-name-nondirectory
                          (directory-file-name worktree-parent))))
         (primary-worktree
          (timfel/agent-shell--create-single-worktree
           repo-root worktree-parent branch)))
    (dolist (sibling-repo (timfel/agent-shell--mx-linked-sibling-repos repo-root))
      (timfel/agent-shell--create-single-worktree
       sibling-repo worktree-parent branch))
    primary-worktree))

(defun timfel/agent-shell--buffer-for-directory (directory)
  "Return the live `agent-shell' buffer rooted at DIRECTORY, or nil."
  (let ((root (file-name-as-directory (expand-file-name directory))))
    (seq-find (lambda (buffer)
                (with-current-buffer buffer
                  (string=
                   (file-name-as-directory
                    (expand-file-name default-directory))
                   root)))
              (agent-shell-buffers))))

(defun timfel/agent-shell--start-shell-in-directory (directory)
  "Start or reuse an `agent-shell' rooted at DIRECTORY."
  (or (timfel/agent-shell--buffer-for-directory directory)
      (let* ((default-directory
              (file-name-as-directory (expand-file-name directory)))
             (config (or (agent-shell--resolve-preferred-config)
                         (user-error
                          "No preferred agent-shell config is available"))))
        (or (cond
             ((fboundp 'agent-shell--start)
              (agent-shell--start :config config
                                  :no-focus t
                                  :new-session t
                                  :session-strategy 'new))
             ((fboundp 'agent-shell-start)
              (agent-shell-start :config config)))
            (timfel/agent-shell--buffer-for-directory directory)
            (error "Could not start agent-shell for %s" directory)))))

(defun timfel/agent-shell--normalize-task-specs (task-specs)
  "Normalize TASK-SPECS into a list of `(TITLE . TASK)' pairs."
  (mapcar (lambda (spec)
            (cond
             ((stringp spec)
              (cons spec spec))
             ((and (consp spec)
                   (stringp (car spec))
                   (stringp (cdr spec)))
              spec)
             (t
              (user-error "Invalid task spec: %S" spec))))
          task-specs))

(defun timfel/agent-shell--rename-shell-buffer (shell-buffer title)
  "Rename SHELL-BUFFER to TITLE using `agent-shell-rename-buffer' when available."
  (with-current-buffer shell-buffer
    (if (fboundp 'agent-shell-rename-buffer)
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) title)))
          (call-interactively #'agent-shell-rename-buffer))
      (rename-buffer title t))))

(defun timfel/agent-shell--queue-startup-requests (shell-buffer task)
  "Queue planning mode in SHELL-BUFFER before TASK, then start processing.

This always enqueues both requests first so the planning prompt is guaranteed
to run before TASK even when the shell has just started and is currently idle."
  (with-current-buffer shell-buffer
    (unless (derived-mode-p 'agent-shell-mode)
      (error "Not in an agent-shell buffer: %s" (buffer-name shell-buffer)))
    (unless (fboundp 'agent-shell--enqueue-request)
      (error "agent-shell does not expose its request queue helper"))
    (agent-shell--enqueue-request :prompt timfel/agent-shell-planning-request)
    (agent-shell--enqueue-request :prompt task)
    (unless (shell-maker-busy)
      (agent-shell-resume-pending-requests))))

;;;###autoload
(defun timfel/agent-shell-fan-out-worktrees (task-specs &optional directory)
  "Create one worktree-backed `agent-shell' per entry in TASK-SPECS.

TASK-SPECS may be either a list of strings, or an alist of `(TITLE . TASK)'
pairs.  Each TASK is queued into its own shell rooted at DIRECTORY's Git
repository, and each shell buffer is renamed to TITLE.  When DIRECTORY is nil,
use `default-directory'.

Interactively, prompt for the number of worktrees, then read a TITLE and TASK
for each one.  Return a plist for each created shell with `:title', `:task',
`:worktree', and `:buffer' keys."
  (interactive
   (list
    (let* ((count (read-number "Number of worktrees/tasks: " 2))
           (task-specs
            (cl-loop for index from 1 to count
                     for title =
                     (string-trim
                      (read-from-minibuffer (format "Title %d: " index)))
                     for task =
                     (string-trim
                      (read-from-minibuffer (format "Task %d: " index) nil nil nil nil title))
                     unless (or (string-empty-p title)
                                (string-empty-p task))
                     collect (cons title task))))
      (unless task-specs
        (user-error "Need at least one non-empty title/task pair"))
      task-specs)
    default-directory))
  (unless (require 'agent-shell nil t)
    (user-error "agent-shell is not installed"))
  (let* ((repo-root (timfel/agent-shell--git-root directory))
         (task-specs (timfel/agent-shell--normalize-task-specs task-specs)))
    (unless repo-root
      (user-error "Not inside a git repository: %s"
                  (expand-file-name (or directory default-directory))))
    (prog1
        (cl-loop for (title . task) in task-specs
                 for index from 1
                 collect
                 (let* ((worktree-dir
                         (timfel/agent-shell--create-worktree
                          repo-root title index))
                        (shell-buffer
                         (timfel/agent-shell--start-shell-in-directory
                          worktree-dir)))
                   (timfel/agent-shell--rename-shell-buffer shell-buffer title)
                   (timfel/agent-shell--queue-startup-requests
                    shell-buffer task)
                   (list :title title
                         :task task
                         :worktree worktree-dir
                         :buffer (buffer-name shell-buffer))))
      (message "Queued %d task(s) across %d worktree shell(s)"
               (length task-specs)
               (length task-specs)))))

(provide 'timfel-agent-shell-extensions)

;;; timfel-agent-shell-extensions.el ends here
