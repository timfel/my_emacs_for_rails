;;; agent-shell-dashboard.el --- Worktree control center for agent-shell -*- lexical-binding: t -*-

;;; Commentary:

;; A standalone tabulated dashboard for agent-shell worktree folders.

;;; Code:

(require 'agent-shell)
(require 'browse-url)
(require 'button)
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)

(declare-function agent-shell-buffers "agent-shell" ())
(declare-function agent-shell-cwd "agent-shell-project" ())
(declare-function agent-shell-rename-buffer "agent-shell" ())
(declare-function agent-shell-select-config "agent-shell" (&key prompt))
(declare-function agent-shell--resolve-preferred-config "agent-shell" ())
(declare-function agent-shell--start "agent-shell" (&rest args))
(declare-function jira-api-call "jira-api" (verb endpoint &rest args))
(declare-function request-response-data "request" (response))
(declare-function shell-maker-busy "shell-maker" ())

(defgroup agent-shell-dashboard nil
  "Dashboard for agent-shell worktree folders."
  :group 'agent-shell)

(defcustom agent-shell-dashboard-worktree-search-directories
  (list user-emacs-directory)
  "Directories searched by `agent-shell-dashboard'.

Each directory is checked for its own `.agent-shell/worktrees' directory
and for one level of child directories containing `.agent-shell/worktrees'."
  :type '(repeat directory)
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-refresh-idle-delay 0.2
  "Initial idle seconds before refreshing a row's remote metadata."
  :type 'number
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-refresh-idle-step 0.15
  "Additional idle seconds added per scheduled row refresh."
  :type 'number
  :group 'agent-shell-dashboard)

(defconst agent-shell-dashboard--buffer-name "*agent-shell dashboard*")

(defconst agent-shell-dashboard--folder-column-width 42)

(defconst agent-shell-dashboard--jira-column-width 16)

(defconst agent-shell-dashboard--pr-regexp
  "\\(?:https?://[^ \t\n\r)>\"]+\\|bitbucket:\\)?/?projects/\\([^/ \t\n\r)>\"]+\\)/repos/\\([^/ \t\n\r)>\"]+\\)/pull-requests/\\([0-9]+\\)")

(defvar-local agent-shell-dashboard--rows nil
  "Hash table mapping row ids to row plists in the current dashboard.")

(defvar-local agent-shell-dashboard--refresh-generation 0
  "Generation counter for incremental dashboard refreshes.")

(defvar-local agent-shell-dashboard--refresh-timers nil
  "Pending incremental dashboard refresh timers.")

(defvar-local agent-shell-dashboard--agent-status-timer nil
  "Timer that updates live agent-shell status icons.")

(defvar agent-shell-cwd-function)
(defvar agent-shell-session-strategy)
(defvar agent-shell--state)
(defvar agent-shell-attention--pending)
(defvar ci-dashboard-base-url)
(defvar jira-base-url)

(defun agent-shell-dashboard--directory (directory)
  "Return DIRECTORY as an expanded directory name."
  (file-name-as-directory (expand-file-name directory)))

(defun agent-shell-dashboard--parent-directory (directory)
  "Return DIRECTORY's parent directory, or nil at the filesystem root."
  (let* ((directory (agent-shell-dashboard--directory directory))
         (parent (file-name-directory (directory-file-name directory))))
    (unless (or (null parent) (equal parent directory))
      (file-name-as-directory parent))))

(defun agent-shell-dashboard--row-id (directory)
  "Return stable row id for DIRECTORY."
  (directory-file-name (agent-shell-dashboard--directory directory)))

(defun agent-shell-dashboard--agent-worktree-folder-p (directory)
  "Return non-nil when DIRECTORY is directly below `.agent-shell/worktrees'."
  (let* ((directory (agent-shell-dashboard--directory directory))
         (parent (agent-shell-dashboard--parent-directory directory))
         (grandparent (and parent
                           (agent-shell-dashboard--parent-directory parent))))
    (and parent grandparent
         (string= (file-name-nondirectory (directory-file-name parent))
                  "worktrees")
         (string= (file-name-nondirectory (directory-file-name grandparent))
                  ".agent-shell"))))

(defun agent-shell-dashboard--worktree-folder-for-directory (directory)
  "Return the `.agent-shell/worktrees/NAME' ancestor for DIRECTORY."
  (let ((probe (agent-shell-dashboard--directory directory))
        found)
    (while (and probe (not found))
      (when (agent-shell-dashboard--agent-worktree-folder-p probe)
        (setq found probe))
      (setq probe (agent-shell-dashboard--parent-directory probe)))
    found))

(defun agent-shell-dashboard--agent-shell-root-for-directory (directory)
  "Return nearest ancestor of DIRECTORY containing `.agent-shell'."
  (when-let ((root (locate-dominating-file directory ".agent-shell")))
    (agent-shell-dashboard--directory root)))

(defun agent-shell-dashboard--directory-subdirs (directory)
  "Return immediate child directories of DIRECTORY."
  (when (file-directory-p directory)
    (seq-filter #'file-directory-p
                (directory-files directory t directory-files-no-dot-files-regexp))))

(defun agent-shell-dashboard--worktree-children (directory)
  "Return worktree child folders below DIRECTORY."
  (when (file-directory-p directory)
    (seq-map #'agent-shell-dashboard--directory
             (agent-shell-dashboard--directory-subdirs directory))))

(defun agent-shell-dashboard--configured-folders ()
  "Return worktree folders found from configured search directories."
  (let (folders)
    (dolist (directory agent-shell-dashboard-worktree-search-directories)
      (let* ((directory (agent-shell-dashboard--directory directory))
             (candidates
              (append
               (list directory)
               (agent-shell-dashboard--directory-subdirs directory))))
        (dolist (candidate candidates)
          (cond
           ((and (file-directory-p candidate)
                 (string= (file-name-nondirectory
                           (directory-file-name candidate))
                          "worktrees")
                 (let ((parent (agent-shell-dashboard--parent-directory candidate)))
                   (and parent
                        (string= (file-name-nondirectory
                                  (directory-file-name parent))
                                 ".agent-shell"))))
            (setq folders
                  (append (agent-shell-dashboard--worktree-children candidate)
                          folders)))
           ((agent-shell-dashboard--agent-worktree-folder-p candidate)
            (push (agent-shell-dashboard--directory candidate) folders))
           (t
            (let ((worktrees (expand-file-name ".agent-shell/worktrees"
                                               candidate)))
              (setq folders
                    (append (agent-shell-dashboard--worktree-children worktrees)
                            folders))))))))
    folders))

(defun agent-shell-dashboard--shell-buffers ()
  "Return live `agent-shell-mode' buffers."
  (if (fboundp 'agent-shell-buffers)
      (seq-filter #'buffer-live-p (agent-shell-buffers))
    (seq-filter (lambda (buffer)
                  (with-current-buffer buffer
                    (derived-mode-p 'agent-shell-mode)))
                (buffer-list))))

(defun agent-shell-dashboard--live-folders ()
  "Return row folders inferred from live agent-shell buffers."
  (delq
   nil
   (mapcar
    (lambda (buffer)
      (with-current-buffer buffer
        (let ((directory (or (ignore-errors (agent-shell-cwd))
                             default-directory)))
          (or (agent-shell-dashboard--worktree-folder-for-directory directory)
              (agent-shell-dashboard--agent-shell-root-for-directory directory)
              (and directory (agent-shell-dashboard--directory directory))))))
    (agent-shell-dashboard--shell-buffers))))

(defun agent-shell-dashboard--discover-folders ()
  "Return de-duplicated dashboard row folders."
  (let ((seen (make-hash-table :test #'equal))
        folders)
    (dolist (folder (append (agent-shell-dashboard--live-folders)
                            (agent-shell-dashboard--configured-folders)))
      (when (and folder (file-directory-p folder))
        (let ((id (agent-shell-dashboard--row-id folder)))
          (unless (gethash id seen)
            (puthash id t seen)
            (push (agent-shell-dashboard--directory folder) folders)))))
    (sort (nreverse folders) #'string-lessp)))

(defun agent-shell-dashboard--buffer-in-folder-p (buffer folder)
  "Return non-nil if BUFFER belongs to dashboard FOLDER."
  (with-current-buffer buffer
    (let ((directory (or (ignore-errors (agent-shell-cwd))
                         default-directory)))
      (and directory
           (file-in-directory-p (agent-shell-dashboard--directory directory)
                                (agent-shell-dashboard--directory folder))))))

(defun agent-shell-dashboard--folder-buffers (folder)
  "Return live agent-shell buffers rooted below FOLDER."
  (seq-filter (lambda (buffer)
                (agent-shell-dashboard--buffer-in-folder-p buffer folder))
              (agent-shell-dashboard--shell-buffers)))

(defun agent-shell-dashboard--tool-call-awaits-permission-p (tool-call)
  "Return non-nil when TOOL-CALL has an unanswered permission request."
  (when tool-call
    (let ((permission-id (or (map-elt tool-call :permission-request-id)
                             (map-elt tool-call 'permissionRequestId)
                             (map-elt tool-call 'permission-request-id)))
          (status (or (map-elt tool-call :status)
                      (map-elt tool-call 'status))))
      (and permission-id
           (or (null status)
               (equal status "pending"))))))

(defun agent-shell-dashboard--permission-pending-p (buffer)
  "Return non-nil when BUFFER has an unanswered permission prompt."
  (with-current-buffer buffer
    (or
     (when (and (boundp 'agent-shell--state) agent-shell--state)
       (let ((tool-calls (map-elt agent-shell--state :tool-calls)))
         (when tool-calls
           (catch 'pending
             (map-do (lambda (_id tool-call)
                       (when (agent-shell-dashboard--tool-call-awaits-permission-p
                              tool-call)
                         (throw 'pending t)))
                     tool-calls)
             nil))))
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (point-min))
         (re-search-forward "Tool Permission" nil t))))))

(defun agent-shell-dashboard--buffer-busy-p (buffer)
  "Return non-nil when BUFFER is busy."
  (with-current-buffer buffer
    (and (fboundp 'shell-maker-busy)
         (ignore-errors (shell-maker-busy)))))

(defun agent-shell-dashboard--attention-pending-entry (buffer)
  "Return BUFFER's `agent-shell-attention' pending entry, or nil."
  (when (and (boundp 'agent-shell-attention--pending)
             (hash-table-p agent-shell-attention--pending))
    (gethash buffer agent-shell-attention--pending)))

(defun agent-shell-dashboard--attention-entry-label (entry)
  "Return the descriptive label stored in attention ENTRY."
  (cond
   ((plistp entry) (plist-get entry :label))
   ((consp entry) (car entry))
   ((stringp entry) entry)))

(defun agent-shell-dashboard--finished-unseen-p (buffer)
  "Return non-nil when BUFFER finished since it was last visited."
  (when-let ((entry (agent-shell-dashboard--attention-pending-entry buffer)))
    (let ((label (agent-shell-dashboard--attention-entry-label entry)))
      (not (and (stringp label)
                (string-prefix-p "Permission:" label))))))

(defun agent-shell-dashboard--agent-state (buffers)
  "Return aggregate agent state for BUFFERS."
  (cond
   ((seq-some #'agent-shell-dashboard--permission-pending-p buffers)
    'permission)
   ((seq-some #'agent-shell-dashboard--buffer-busy-p buffers)
    'busy)
   ((seq-some #'agent-shell-dashboard--finished-unseen-p buffers)
    'finished-unseen)
   (buffers
    'idle)
   (t
    'none)))

(defun agent-shell-dashboard--agent-icon (state)
  "Return a propertized icon for aggregate STATE."
  (pcase state
    ('permission
     (propertize "⁈" 'face 'error 'help-echo "agent-shell is waiting for permission"))
    ('busy
     (propertize "τ" 'face 'warning 'help-echo "agent-shell is busy"))
    ('finished-unseen
     (propertize "⁇" 'face 'warning
                 'help-echo "agent-shell finished since last visit"))
    ('idle
     (propertize "✓" 'face 'success 'help-echo "agent-shell is idle"))
    (_
     (propertize "∅" 'face 'shadow 'help-echo "no live agent-shell"))))

(defun agent-shell-dashboard--run-string-sync (program &rest args)
  "Run PROGRAM with ARGS and return stdout as a string.
Signal an error when the process exits non-zero."
  (unless (executable-find program)
    (error "Executable not found: %s" program))
  (with-temp-buffer
    (let ((exit (apply #'process-file program nil t nil args)))
      (unless (zerop exit)
        (error "%s %s failed: %s"
               program
               (string-join args " ")
               (string-trim (buffer-string))))
      (buffer-string))))

(defun agent-shell-dashboard--run-string
    (program args callback &optional error-callback)
  "Run PROGRAM with ARGS asynchronously and call CALLBACK with stdout.
When the command fails, call ERROR-CALLBACK with an error string if
non-nil, otherwise log the error with `message'."
  (if (not (executable-find program))
      (let ((error (format "Executable not found: %s" program)))
        (if error-callback
            (funcall error-callback error)
          (message "%s" error)))
    (let* ((buffer (generate-new-buffer
                    (format " *agent-shell-dashboard-%s*" program)))
           (command (cons program args))
           (process
            (make-process
             :name (format "agent-shell-dashboard-%s" program)
             :buffer buffer
             :command command
             :noquery t
             :sentinel
             (lambda (process _event)
               (when (memq (process-status process) '(exit signal))
                 (let* ((exit (process-exit-status process))
                        (output (when (buffer-live-p buffer)
                                  (with-current-buffer buffer
                                    (buffer-string))))
                        (failed (not (and (eq (process-status process) 'exit)
                                          (zerop exit)))))
                   (when (buffer-live-p buffer)
                     (kill-buffer buffer))
                   (if failed
                       (let ((error
                              (format "%s %s failed: %s"
                                      program
                                      (string-join args " ")
                                      (string-trim (or output "")))))
                         (if error-callback
                             (funcall error-callback error)
                           (message "%s" error)))
                     (funcall callback (or output "")))))))))
      process)))

(defun agent-shell-dashboard--run-string-or-nil (program &rest args)
  "Run PROGRAM with ARGS and return stdout, or nil on failure."
  (condition-case nil
      (string-trim (apply #'agent-shell-dashboard--run-string-sync program args))
    (error nil)))

(defun agent-shell-dashboard--json-from-string (string)
  "Parse first JSON object or array found in STRING."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false :false)
        (json-null nil))
    (when (string-match "[[{]" string)
      (json-read-from-string (substring string (match-beginning 0))))))

(defun agent-shell-dashboard--run-json-sync (program &rest args)
  "Run PROGRAM with ARGS and parse JSON output."
  (agent-shell-dashboard--json-from-string
   (apply #'agent-shell-dashboard--run-string-sync program args)))

(defun agent-shell-dashboard--run-json
    (program args callback &optional error-callback)
  "Run PROGRAM with ARGS asynchronously and call CALLBACK with JSON."
  (agent-shell-dashboard--run-string
   program args
   (lambda (output)
     (condition-case err
         (funcall callback
                  (agent-shell-dashboard--json-from-string output))
       (error
        (let ((error (error-message-string err)))
          (if error-callback
              (funcall error-callback error)
            (message "%s" error))))))
   error-callback))

(defun agent-shell-dashboard--git-directories (folder)
  "Return FOLDER and one-level children that look like Git working trees."
  (seq-filter
   (lambda (directory)
     (file-exists-p (expand-file-name ".git" directory)))
   (cons (agent-shell-dashboard--directory folder)
         (agent-shell-dashboard--directory-subdirs folder))))

(defun agent-shell-dashboard--git-output (directory &rest args)
  "Run git in DIRECTORY with ARGS and return trimmed stdout or nil."
  (apply #'agent-shell-dashboard--run-string-or-nil
         "git" "-C" (agent-shell-dashboard--directory directory) args))

(defun agent-shell-dashboard--git-branch (directory)
  "Return current Git branch in DIRECTORY, or nil."
  (agent-shell-dashboard--git-output directory
                                    "symbolic-ref" "--quiet" "--short" "HEAD"))

(defun agent-shell-dashboard--git-remote (directory)
  "Return origin remote URL in DIRECTORY, or nil."
  (agent-shell-dashboard--git-output directory
                                    "config" "--get" "remote.origin.url"))

(defun agent-shell-dashboard--git-head (directory)
  "Return HEAD commit in DIRECTORY, or nil."
  (agent-shell-dashboard--git-output directory
                                    "rev-parse" "--verify" "HEAD"))

(defun agent-shell-dashboard--git-remote-coordinates-from-url (remote)
  "Infer Bitbucket project/repo coordinates from REMOTE URL."
  (cond
   ((string-match "/projects/\\([^/]+\\)/repos/\\([^/.]+\\)" remote)
    (list :project (match-string 1 remote)
          :repo (match-string 2 remote)))
   ((string-match "/scm/\\([^/]+\\)/\\([^/.]+\\)\\(?:\\.git\\)?\\'" remote)
    (list :project (upcase (match-string 1 remote))
          :repo (match-string 2 remote)))
   ((string-match "[:/]\\([^/:]+\\)/\\([^/.]+\\)\\(?:\\.git\\)?\\'" remote)
    (list :project (upcase (match-string 1 remote))
          :repo (match-string 2 remote)))))

(defun agent-shell-dashboard--git-remote-coordinates (directory)
  "Infer Bitbucket project/repo coordinates from DIRECTORY's origin URL."
  (when-let ((remote (agent-shell-dashboard--git-remote directory)))
    (agent-shell-dashboard--git-remote-coordinates-from-url remote)))

(defun agent-shell-dashboard--agent-shell-candidates (folder)
  "Return launch directories below FOLDER that contain `.agent-shell'."
  (let ((folder (agent-shell-dashboard--directory folder)))
    (cond
     ((file-directory-p (expand-file-name ".agent-shell" folder))
      (list folder))
     (t
      (seq-filter
       (lambda (directory)
         (file-directory-p (expand-file-name ".agent-shell" directory)))
       (agent-shell-dashboard--directory-subdirs folder))))))

(defun agent-shell-dashboard--launch-directory (folder &optional prompt)
  "Return agent-shell launch directory for FOLDER.
When PROMPT is non-nil, ask if multiple one-level candidates exist."
  (let ((candidates (agent-shell-dashboard--agent-shell-candidates folder)))
    (pcase candidates
      ('nil
       (let ((git-candidates (agent-shell-dashboard--git-directories folder)))
         (pcase git-candidates
           ('nil (agent-shell-dashboard--directory folder))
           (`(,only) only)
           (_ (if prompt
                  (agent-shell-dashboard--directory
                   (completing-read
                    "Launch agent-shell in Git worktree: "
                    (mapcar #'agent-shell-dashboard--directory git-candidates)
                    nil t))
                (car git-candidates))))))
      (`(,only) only)
      (_ (if prompt
             (agent-shell-dashboard--directory
              (completing-read
               "Launch agent-shell in: "
               (mapcar #'agent-shell-dashboard--directory candidates)
               nil t))
           (car candidates))))))

(defun agent-shell-dashboard--push-unique (item key-fn items)
  "Add ITEM to ITEMS when KEY-FN does not already identify an existing item."
  (let ((key (funcall key-fn item)))
    (if-let ((existing (seq-find (lambda (other)
                                   (equal key (funcall key-fn other)))
                                 items)))
        (progn
          (when (and (not (plist-get existing :title))
                     (plist-get item :title))
            (setq existing (plist-put existing :title (plist-get item :title))))
          items)
      (cons item items))))

(defun agent-shell-dashboard--canonical-jira-key (key)
  "Return KEY in the canonical dashboard form."
  (upcase key))

(defun agent-shell-dashboard--extract-jiras-from-name (name)
  "Return Jira entries found in worktree or branch NAME."
  (let ((case-fold-search nil)
        (regexp "\\(?:\\`\\|[^[:alnum:]]\\)\\([Gg][Rr][Aa][Aa][Ll][Oo][Ss]-[0-9]+\\|[Gg][Rr]-[0-9]+\\)\\(?:\\'\\|[^[:alnum:]]\\)")
        entries)
    (with-temp-buffer
      (insert name)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let* ((key (agent-shell-dashboard--canonical-jira-key
                     (match-string 1)))
               (entry (list :key key)))
          (setq entries
                (agent-shell-dashboard--push-unique
                 entry (lambda (jira) (plist-get jira :key)) entries)))))
    (nreverse entries)))

(defun agent-shell-dashboard--extract-prs-from-text (text)
  "Return PR entries found in TEXT."
  (let (entries)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward agent-shell-dashboard--pr-regexp nil t)
        (let* ((project (match-string 1))
               (repo (match-string 2))
               (id (match-string 3))
               (url (match-string 0))
               (entry (list :project project
                            :repo repo
                            :id id
                            :url (if (string-prefix-p "http" url)
                                     url
                                   nil))))
          (setq entries
                (agent-shell-dashboard--push-unique
                 entry #'agent-shell-dashboard--pr-key entries)))))
    (nreverse entries)))

(defun agent-shell-dashboard--pr-key (pr)
  "Return uniqueness key for PR plist."
  (format "%s/%s#%s"
          (or (plist-get pr :project) "")
          (or (plist-get pr :repo) "")
          (or (plist-get pr :id) "")))

(defun agent-shell-dashboard--extract-local-links (folder)
  "Return plist with local Jira and PR links inferred from FOLDER."
  (let ((jiras (agent-shell-dashboard--extract-jiras-from-name
                (file-name-nondirectory (directory-file-name folder))))
        (prs nil))
    (dolist (directory (agent-shell-dashboard--git-directories folder))
      (when-let ((branch (agent-shell-dashboard--git-branch directory)))
        (setq jiras
              (seq-reduce
               (lambda (items jira)
                 (agent-shell-dashboard--push-unique
                  jira (lambda (item) (plist-get item :key)) items))
               (agent-shell-dashboard--extract-jiras-from-name branch)
               jiras)))
      (when-let* ((coords (agent-shell-dashboard--git-remote-coordinates directory))
                  (branch (agent-shell-dashboard--git-branch directory)))
        (when (string-match "\\bPR[-_]?\\([0-9]+\\)\\b" branch)
          (push (append coords
                        (list :id (match-string 1 branch)))
                prs))))
    (list :jiras (nreverse jiras)
          :prs (nreverse prs))))

(defun agent-shell-dashboard--jira-url (key)
  "Return Jira browser URL for KEY when `jira-base-url' is known."
  (when (and (boundp 'jira-base-url)
             (stringp jira-base-url)
             (not (string-empty-p jira-base-url)))
    (format "%s/browse/%s" (string-remove-suffix "/" jira-base-url) key)))

(defun agent-shell-dashboard--jira-info-from-data (key data)
  "Convert Jira DATA for KEY to a Jira entry plist."
  (let* ((fields (map-elt data 'fields))
         (status (map-elt fields 'status))
         (resolution (map-elt fields 'resolution)))
    (list :key key
          :title (map-elt fields 'summary)
          :status (map-elt status 'name)
          :resolution (map-elt resolution 'name)
          :url (or (map-elt data 'url)
                   (agent-shell-dashboard--jira-url key)))))

(defun agent-shell-dashboard--fetch-jira-via-api (key)
  "Fetch Jira KEY through jira.el, returning a Jira entry plist."
  (when (require 'jira-api nil t)
    (let* ((response
            (jira-api-call
             "GET" (format "issue/%s" key)
             :params '(("fields" . "summary,status,resolution"))
             :sync t))
           (data (request-response-data response)))
      (agent-shell-dashboard--jira-info-from-data key data))))

(defun agent-shell-dashboard--fetch-jira-via-gdev (key)
  "Fetch Jira KEY through gdev-cli, returning a Jira entry plist."
  (when (executable-find "gdev-cli")
    (let ((data (agent-shell-dashboard--run-json-sync
                 "gdev-cli" "jira" "get-issue"
                 "--json" "--skip-cache" (concat "-id=" key))))
      (agent-shell-dashboard--jira-info-from-data key data))))

(defun agent-shell-dashboard--fetch-jira (key)
  "Fetch Jira KEY status/title."
  (condition-case err
      (or (condition-case nil
              (agent-shell-dashboard--fetch-jira-via-api key)
            (error nil))
          (agent-shell-dashboard--fetch-jira-via-gdev key))
    (error
     (list :key key
           :status "error"
           :error (error-message-string err)))))

(defun agent-shell-dashboard--json-map-p (object)
  "Return non-nil when OBJECT is an alist-like JSON object."
  (and (consp object)
       (consp (car object))
       (or (symbolp (caar object))
           (stringp (caar object)))))

(defun agent-shell-dashboard--find-pr-in-json (object id)
  "Return PR object with numeric/string ID in OBJECT."
  (cond
   ((agent-shell-dashboard--json-map-p object)
    (or (when (equal (format "%s" (map-elt object 'id)) id)
          object)
        (when-let ((pull-request (map-elt object 'pullRequest)))
          (agent-shell-dashboard--find-pr-in-json pull-request id))
        (seq-some (lambda (pair)
                    (agent-shell-dashboard--find-pr-in-json (cdr pair) id))
                  object)))
   ((listp object)
    (seq-some (lambda (item)
                (agent-shell-dashboard--find-pr-in-json item id))
              object))
   ((vectorp object)
    (seq-some (lambda (item)
                (agent-shell-dashboard--find-pr-in-json item id))
              (append object nil)))))

(defun agent-shell-dashboard--bitbucket-pr-object-p (object)
  "Return non-nil when OBJECT looks like a Bitbucket PR object."
  (and (agent-shell-dashboard--json-map-p object)
       (map-elt object 'id)
       (or (map-elt object 'fromRef)
           (map-elt object 'toRef)
           (map-elt object 'title))))

(defun agent-shell-dashboard--collect-pr-objects (object)
  "Return all Bitbucket PR-like objects inside OBJECT."
  (cond
   ((agent-shell-dashboard--json-map-p object)
    (let ((children
           (apply #'append
                  (delq nil
                        (mapcar (lambda (pair)
                                  (agent-shell-dashboard--collect-pr-objects
                                   (cdr pair)))
                                object)))))
      (if (agent-shell-dashboard--bitbucket-pr-object-p object)
          (cons object children)
        children)))
   ((listp object)
    (apply #'append
           (delq nil
                 (mapcar #'agent-shell-dashboard--collect-pr-objects object))))
   ((vectorp object)
    (agent-shell-dashboard--collect-pr-objects (append object nil)))))

(defun agent-shell-dashboard--bitbucket-branch-name (ref)
  "Return branch display name from Bitbucket REF."
  (when (agent-shell-dashboard--json-map-p ref)
    (when-let ((name (or (map-elt ref 'displayId)
                         (map-elt ref 'id))))
      (when (stringp name)
        (string-remove-prefix "refs/heads/" name)))))

(defun agent-shell-dashboard--bitbucket-pr-commit (object)
  "Return latest commit from Bitbucket PR OBJECT."
  (or (map-elt object 'latestCommit)
      (map-elt object 'lastCommit)
      (when-let ((from-ref (map-elt object 'fromRef)))
        (or (map-elt from-ref 'latestCommit)
            (map-elt from-ref 'lastCommit)
            (map-elt from-ref 'latestChangeset)))))

(defun agent-shell-dashboard--pr-from-object (project repo object)
  "Return dashboard PR plist from Bitbucket PR OBJECT."
  (when-let ((id (map-elt object 'id)))
    (append
     (list :project project
           :repo repo
           :id (format "%s" id)
           :branch (agent-shell-dashboard--bitbucket-branch-name
                    (map-elt object 'fromRef))
           :latest-commit (agent-shell-dashboard--bitbucket-pr-commit object))
     (agent-shell-dashboard--pr-info-from-data
      (list :project project :repo repo :id (format "%s" id))
      object))))

(defun agent-shell-dashboard--fetch-repo-prs (project repo &optional filter-text)
  "Return PR plists for PROJECT and REPO from Bitbucket.
When FILTER-TEXT is non-nil, ask Bitbucket to filter PRs by that text."
  (let* ((args (append
                (list "bitbucket" "list-prs"
                      "--json"
                      (concat "--project=" project)
                      (concat "--repos=" repo)
                      "--state=ALL"
                      "--pageSize=100")
                (when filter-text
                  (list (concat "--filterText=" filter-text)))))
         (data (apply #'agent-shell-dashboard--run-json-sync
                      "gdev-cli" args))
         (objects (agent-shell-dashboard--collect-pr-objects data)))
    (delq nil
          (mapcar (lambda (object)
                    (agent-shell-dashboard--pr-from-object
                     project repo object))
                  objects))))

(defun agent-shell-dashboard--repo-prs (project repo cache &optional filter-text)
  "Return cached Bitbucket PRs for PROJECT and REPO using CACHE.
When FILTER-TEXT is non-nil, cache the filtered query separately."
  (let ((key (format "%s/%s:%s" project repo (or filter-text ""))))
    (agent-shell-dashboard--cache-get-or-put
     cache key
     (lambda ()
       (condition-case nil
           (agent-shell-dashboard--fetch-repo-prs project repo filter-text)
         (error nil))))))

(defun agent-shell-dashboard--pr-matches-branch-p (pr branch head)
  "Return non-nil when PR matches local BRANCH or HEAD."
  (or (and branch
           (stringp (plist-get pr :branch))
           (string= branch (plist-get pr :branch)))
      (and head
           (stringp (plist-get pr :latest-commit))
           (string= head (plist-get pr :latest-commit)))))

(defun agent-shell-dashboard--pr-search-text (pr)
  "Return searchable text for PR."
  (string-join
   (delq nil
         (list (plist-get pr :title)
               (plist-get pr :description)
               (plist-get pr :branch)))
   "\n"))

(defun agent-shell-dashboard--text-contains-jira-p (text key)
  "Return non-nil when TEXT contains Jira KEY."
  (let ((case-fold-search t))
    (string-match-p
     (format "\\(?:\\`\\|[^[:alnum:]]\\)%s\\(?:\\'\\|[^[:alnum:]]\\)"
             (regexp-quote key))
     text)))

(defun agent-shell-dashboard--pr-matches-jiras-p (pr jiras)
  "Return non-nil when PR text mentions one of JIRAS."
  (let ((text (agent-shell-dashboard--pr-search-text pr)))
    (seq-some
     (lambda (jira)
       (when-let ((key (plist-get jira :key)))
         (agent-shell-dashboard--text-contains-jira-p text key)))
     jiras)))

(defun agent-shell-dashboard--push-matching-prs
    (prs branch head jiras items)
  "Add PRS matching BRANCH, HEAD, or JIRAS to ITEMS."
  (seq-reduce
   (lambda (acc pr)
     (if (or (agent-shell-dashboard--pr-matches-branch-p pr branch head)
             (agent-shell-dashboard--pr-matches-jiras-p pr jiras))
         (agent-shell-dashboard--push-unique
          pr #'agent-shell-dashboard--pr-key acc)
       acc))
   prs
   items))

(defun agent-shell-dashboard--discover-branch-prs (folder jiras cache)
  "Discover PRs for Git branches and JIRAS below FOLDER using CACHE."
  (let (prs)
    (dolist (directory (agent-shell-dashboard--git-directories folder))
      (when-let* ((coords (agent-shell-dashboard--git-remote-coordinates directory))
                  (project (plist-get coords :project))
                  (repo (plist-get coords :repo)))
        (let ((branch (agent-shell-dashboard--git-branch directory))
              (head (agent-shell-dashboard--git-head directory)))
          (dolist (pr (agent-shell-dashboard--repo-prs project repo cache))
            (setq prs
                  (agent-shell-dashboard--push-matching-prs
                   (list pr) branch head jiras prs)))
          (dolist (jira jiras)
            (when-let ((key (plist-get jira :key)))
              (setq prs
                    (agent-shell-dashboard--push-matching-prs
                     (agent-shell-dashboard--repo-prs project repo cache key)
                     branch head jiras prs)))))))
    (nreverse prs)))

(defun agent-shell-dashboard--first-http-url (object)
  "Return the first HTTP URL found in OBJECT."
  (cond
   ((and (stringp object)
         (string-prefix-p "http" object))
    object)
   ((agent-shell-dashboard--json-map-p object)
    (or (agent-shell-dashboard--first-http-url (map-elt object 'href))
        (seq-some (lambda (pair)
                    (agent-shell-dashboard--first-http-url (cdr pair)))
                  object)))
   ((listp object)
    (seq-some #'agent-shell-dashboard--first-http-url object))
   ((vectorp object)
    (seq-some #'agent-shell-dashboard--first-http-url (append object nil)))))

(defun agent-shell-dashboard--pr-url (pr)
  "Return browser URL for PR plist."
  (or (agent-shell-dashboard--first-http-url (plist-get pr :url))
      (when (and (plist-get pr :project)
                 (plist-get pr :repo)
                 (plist-get pr :id)
                 (boundp 'ci-dashboard-base-url)
                 (stringp ci-dashboard-base-url)
                 (not (string-empty-p ci-dashboard-base-url)))
        (format "%s/projects/%s/repos/%s/pull-requests/%s"
                (string-remove-suffix "/" ci-dashboard-base-url)
                (plist-get pr :project)
                (plist-get pr :repo)
                (plist-get pr :id)))
      (when (and (plist-get pr :project)
                 (plist-get pr :repo)
                 (plist-get pr :id))
        (format "bitbucket:projects/%s/repos/%s/pull-requests/%s"
                (plist-get pr :project)
                (plist-get pr :repo)
                (plist-get pr :id)))))

(defun agent-shell-dashboard--pr-info-from-data (pr data)
  "Convert Bitbucket DATA to a PR entry merged with PR."
  (let ((object (or (agent-shell-dashboard--find-pr-in-json
                     data (plist-get pr :id))
                    data)))
    (append
     (list :title (map-elt object 'title)
           :description (map-elt object 'description)
           :status (or (map-elt object 'state)
                       (map-elt object 'status))
           :url (or (agent-shell-dashboard--first-http-url
                     (plist-get pr :url))
                    (agent-shell-dashboard--first-http-url
                     (map-elt object 'links))))
     pr)))

(defun agent-shell-dashboard--fetch-pr (pr)
  "Fetch status/title for PR plist."
  (condition-case err
      (let* ((project (or (plist-get pr :project)
                          (error "Missing Bitbucket project")))
             (repo (or (plist-get pr :repo)
                       (error "Missing Bitbucket repo")))
             (data (agent-shell-dashboard--run-json-sync
                    "gdev-cli" "bitbucket" "list-prs"
                    "--json"
                    (concat "--project=" project)
                    (concat "--repos=" repo)
                    "--state=ALL"
                    "--pageSize=100")))
        (agent-shell-dashboard--pr-info-from-data pr data))
    (error
     (append (list :status "error"
                   :error (error-message-string err))
             pr))))

(defun agent-shell-dashboard--cache-get-or-put (cache key thunk)
  "Return CACHE value for KEY, computing it with THUNK if missing."
  (let ((missing (make-symbol "missing")))
    (let ((value (gethash key cache missing)))
      (if (eq value missing)
          (let ((computed (funcall thunk)))
            (puthash key computed cache)
            computed)
        value))))

(defun agent-shell-dashboard--merge-fetched-jiras (jiras cache)
  "Fetch and merge JIRAS using CACHE."
  (mapcar
   (lambda (jira)
     (let* ((key (plist-get jira :key))
            (fetched (agent-shell-dashboard--cache-get-or-put
                      cache key
                      (lambda () (agent-shell-dashboard--fetch-jira key)))))
       (append fetched jira)))
   jiras))

(defun agent-shell-dashboard--merge-fetched-prs (prs cache)
  "Fetch and merge PRS using CACHE."
  (mapcar
   (lambda (pr)
     (let* ((key (agent-shell-dashboard--pr-key pr))
            (fetched (if (and (plist-get pr :title)
                              (plist-get pr :status))
                         pr
                       (agent-shell-dashboard--cache-get-or-put
                        cache key
                        (lambda () (agent-shell-dashboard--fetch-pr pr))))))
       (append fetched pr)))
   prs))

(defun agent-shell-dashboard--async-map (items worker callback)
  "Call WORKER for each item in ITEMS and collect results in CALLBACK.
WORKER receives an item and a per-item callback."
  (if (null items)
      (funcall callback nil)
    (let ((remaining (length items))
          (results (make-vector (length items) nil)))
      (cl-loop
       for item in items
       for index from 0
       do (let ((slot index))
            (funcall
             worker item
             (lambda (result)
               (aset results slot result)
               (setq remaining (1- remaining))
               (when (zerop remaining)
                 (funcall callback (append results nil))))))))))

(defun agent-shell-dashboard--cache-get-or-put-async (cache key producer callback)
  "Return async CACHE value for KEY, computing it with PRODUCER.
PRODUCER receives a callback.  CALLBACK receives the cached or
computed value."
  (let* ((missing (make-symbol "missing"))
         (entry (gethash key cache missing)))
    (cond
     ((eq entry missing)
      (puthash key (list :state 'pending :callbacks (list callback)) cache)
      (funcall
       producer
       (lambda (value)
         (let* ((entry (gethash key cache))
                (callbacks (plist-get entry :callbacks)))
           (puthash key (list :state 'done :value value) cache)
           (dolist (callback (nreverse callbacks))
             (funcall callback value))))))
     ((eq (plist-get entry :state) 'pending)
      (puthash key
               (plist-put entry :callbacks
                          (cons callback (plist-get entry :callbacks)))
               cache))
     ((eq (plist-get entry :state) 'done)
      (funcall callback (plist-get entry :value)))
     (t
      (funcall callback entry)))))

(defun agent-shell-dashboard--git-output-async (directory args callback)
  "Run git in DIRECTORY with ARGS and call CALLBACK with trimmed stdout or nil."
  (agent-shell-dashboard--run-string
   "git"
   (append (list "-C" (agent-shell-dashboard--directory directory)) args)
   (lambda (output)
     (funcall callback (string-trim output)))
   (lambda (_error)
     (funcall callback nil))))

(defun agent-shell-dashboard--git-directory-info-async (folder callback)
  "Call CALLBACK with Git info plists for FOLDER and one-level children."
  (agent-shell-dashboard--async-map
   (agent-shell-dashboard--git-directories folder)
   (lambda (directory done)
     (let ((branch nil)
           (remote nil)
           (head nil)
           (remaining 3))
       (cl-labels ((finish ()
                     (setq remaining (1- remaining))
                     (when (zerop remaining)
                       (funcall
                        done
                        (list :directory directory
                              :branch branch
                              :remote remote
                              :coords (and remote
                                           (agent-shell-dashboard--git-remote-coordinates-from-url
                                            remote))
                              :head head)))))
         (agent-shell-dashboard--git-output-async
          directory '("symbolic-ref" "--quiet" "--short" "HEAD")
          (lambda (value)
            (setq branch value)
            (finish)))
         (agent-shell-dashboard--git-output-async
          directory '("config" "--get" "remote.origin.url")
          (lambda (value)
            (setq remote value)
            (finish)))
         (agent-shell-dashboard--git-output-async
          directory '("rev-parse" "--verify" "HEAD")
          (lambda (value)
            (setq head value)
            (finish))))))
   callback))

(defun agent-shell-dashboard--links-from-git-info (folder infos)
  "Return local Jira and PR links for FOLDER using Git INFOS."
  (let ((jiras (agent-shell-dashboard--extract-jiras-from-name
                (file-name-nondirectory (directory-file-name folder))))
        (prs nil))
    (dolist (info infos)
      (when-let ((branch (plist-get info :branch)))
        (setq jiras
              (seq-reduce
               (lambda (items jira)
                 (agent-shell-dashboard--push-unique
                  jira (lambda (item) (plist-get item :key)) items))
               (agent-shell-dashboard--extract-jiras-from-name branch)
               jiras))
        (when-let ((coords (plist-get info :coords)))
          (when (string-match "\\bPR[-_]?\\([0-9]+\\)\\b" branch)
            (push (append coords
                          (list :id (match-string 1 branch)))
                  prs)))))
    (list :jiras (nreverse jiras)
          :prs (nreverse prs))))

(defun agent-shell-dashboard--fetch-jira-async (key cache callback)
  "Fetch Jira KEY asynchronously with CACHE and call CALLBACK with a plist."
  (agent-shell-dashboard--cache-get-or-put-async
   cache key
   (lambda (done)
     (if (not (executable-find "gdev-cli"))
         (funcall done (list :key key))
       (agent-shell-dashboard--run-json
        "gdev-cli"
        (list "jira" "get-issue" "--json" "--skip-cache" (concat "-id=" key))
        (lambda (data)
          (condition-case err
              (funcall done
                       (agent-shell-dashboard--jira-info-from-data key data))
            (error
             (funcall done
                      (list :key key
                            :status "error"
                            :error (error-message-string err))))))
        (lambda (error)
          (funcall done
                   (list :key key
                         :status "error"
                         :error error))))))
   callback))

(defun agent-shell-dashboard--fetch-repo-prs-async
    (project repo filter-text callback)
  "Fetch PR plists for PROJECT and REPO asynchronously.
When FILTER-TEXT is non-nil, ask Bitbucket to filter PRs by it."
  (let ((args (append
               (list "bitbucket" "list-prs"
                     "--json"
                     (concat "--project=" project)
                     (concat "--repos=" repo)
                     "--state=ALL"
                     "--pageSize=100")
               (when filter-text
                 (list (concat "--filterText=" filter-text))))))
    (agent-shell-dashboard--run-json
     "gdev-cli" args
     (lambda (data)
       (let ((objects (agent-shell-dashboard--collect-pr-objects data)))
         (funcall
          callback
          (delq nil
                (mapcar (lambda (object)
                          (agent-shell-dashboard--pr-from-object
                           project repo object))
                        objects)))))
     (lambda (_error)
       (funcall callback nil)))))

(defun agent-shell-dashboard--repo-prs-async
    (project repo cache filter-text callback)
  "Return cached async Bitbucket PRs for PROJECT and REPO."
  (let ((key (format "%s/%s:%s" project repo (or filter-text ""))))
    (agent-shell-dashboard--cache-get-or-put-async
     cache key
     (lambda (done)
       (agent-shell-dashboard--fetch-repo-prs-async
        project repo filter-text done))
     callback)))

(defun agent-shell-dashboard--discover-branch-prs-async
    (infos jiras cache callback)
  "Discover PRs for Git INFOS and JIRAS asynchronously using CACHE."
  (let (requests)
    (dolist (info infos)
      (when-let* ((coords (plist-get info :coords))
                  (project (plist-get coords :project))
                  (repo (plist-get coords :repo)))
        (push (list :project project
                    :repo repo
                    :filter nil
                    :branch (plist-get info :branch)
                    :head (plist-get info :head))
              requests)
        (dolist (jira jiras)
          (when-let ((key (plist-get jira :key)))
            (push (list :project project
                        :repo repo
                        :filter key
                        :branch (plist-get info :branch)
                        :head (plist-get info :head))
                  requests)))))
    (agent-shell-dashboard--async-map
     requests
     (lambda (request done)
       (agent-shell-dashboard--repo-prs-async
        (plist-get request :project)
        (plist-get request :repo)
        cache
        (plist-get request :filter)
        (lambda (prs)
          (funcall
           done
           (agent-shell-dashboard--push-matching-prs
            prs
            (plist-get request :branch)
            (plist-get request :head)
            jiras
            nil)))))
     (lambda (matches)
       (let (prs)
         (dolist (group matches)
           (dolist (pr group)
             (setq prs
                   (agent-shell-dashboard--push-unique
                    pr #'agent-shell-dashboard--pr-key prs))))
         (funcall callback (nreverse prs)))))))

(defun agent-shell-dashboard--merge-fetched-jiras-async
    (jiras cache callback)
  "Fetch and merge JIRAS asynchronously using CACHE."
  (agent-shell-dashboard--async-map
   jiras
   (lambda (jira done)
     (if-let ((key (plist-get jira :key)))
         (agent-shell-dashboard--fetch-jira-async
          key cache
          (lambda (fetched)
            (funcall done (append fetched jira))))
       (funcall done jira)))
   callback))

(defun agent-shell-dashboard--fetch-pr-async
    (pr pr-cache repo-pr-cache callback)
  "Fetch status/title for PR asynchronously using CACHES."
  (let ((key (agent-shell-dashboard--pr-key pr)))
    (agent-shell-dashboard--cache-get-or-put-async
     pr-cache key
     (lambda (done)
       (let ((project (plist-get pr :project))
             (repo (plist-get pr :repo))
             (id (plist-get pr :id)))
         (if (not (and project repo id))
             (funcall done
                      (append (list :status "error"
                                    :error "Missing Bitbucket project/repo/id")
                              pr))
           (agent-shell-dashboard--repo-prs-async
            project repo repo-pr-cache nil
            (lambda (prs)
              (funcall
               done
               (or (seq-find
                    (lambda (candidate)
                      (equal (plist-get candidate :id) id))
                    prs)
                   (append (list :status "error"
                                 :error "PR not found")
                           pr))))))))
     callback)))

(defun agent-shell-dashboard--merge-fetched-prs-async
    (prs pr-cache repo-pr-cache callback)
  "Fetch and merge PRS asynchronously using CACHES."
  (agent-shell-dashboard--async-map
   prs
   (lambda (pr done)
     (if (and (plist-get pr :title)
              (plist-get pr :status))
         (funcall done pr)
       (agent-shell-dashboard--fetch-pr-async
        pr pr-cache repo-pr-cache
        (lambda (fetched)
          (funcall done (append fetched pr))))))
   callback))

(defun agent-shell-dashboard--make-row-async
    (folder jira-cache pr-cache repo-pr-cache callback)
  "Build dashboard row for FOLDER asynchronously and call CALLBACK."
  (let ((buffers (agent-shell-dashboard--folder-buffers folder))
        (state nil))
    (setq state (agent-shell-dashboard--agent-state buffers))
    (agent-shell-dashboard--git-directory-info-async
     folder
     (lambda (infos)
       (let* ((links (agent-shell-dashboard--links-from-git-info folder infos))
              (jiras (plist-get links :jiras))
              (prs (plist-get links :prs)))
         (agent-shell-dashboard--discover-branch-prs-async
          infos jiras repo-pr-cache
          (lambda (branch-prs)
            (setq prs
                  (seq-reduce
                   (lambda (items pr)
                     (agent-shell-dashboard--push-unique
                      pr #'agent-shell-dashboard--pr-key items))
                   branch-prs
                   prs))
            (agent-shell-dashboard--merge-fetched-jiras-async
             jiras jira-cache
             (lambda (fetched-jiras)
               (agent-shell-dashboard--merge-fetched-prs-async
                prs pr-cache repo-pr-cache
                (lambda (fetched-prs)
                  (funcall
                   callback
                   (list :folder (agent-shell-dashboard--directory folder)
                         :buffers buffers
                         :agent-state state
                         :jiras fetched-jiras
                         :prs fetched-prs)))))))))))))

(defun agent-shell-dashboard--make-row
    (folder &optional full-refresh jira-cache pr-cache repo-pr-cache)
  "Build row plist for FOLDER.
When FULL-REFRESH is non-nil, refresh Jira and PR metadata using CACHES."
  (let* ((buffers (agent-shell-dashboard--folder-buffers folder))
         (state (agent-shell-dashboard--agent-state buffers))
         (links (agent-shell-dashboard--extract-local-links folder))
         (jiras (plist-get links :jiras))
         (prs (plist-get links :prs)))
    (when full-refresh
      (setq prs
            (seq-reduce
             (lambda (items pr)
               (agent-shell-dashboard--push-unique
                pr #'agent-shell-dashboard--pr-key items))
             (agent-shell-dashboard--discover-branch-prs
              folder jiras
              (or repo-pr-cache
                  (make-hash-table :test #'equal)))
             prs))
      (setq jiras (agent-shell-dashboard--merge-fetched-jiras
                   jiras jira-cache))
      (setq prs (agent-shell-dashboard--merge-fetched-prs prs pr-cache)))
    (list :folder (agent-shell-dashboard--directory folder)
          :buffers buffers
          :agent-state state
          :jiras jiras
          :prs prs)))

(defun agent-shell-dashboard--link-text (text url)
  "Return TEXT propertized as a clickable URL link."
  (if (and url (not (string-empty-p url)))
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1]
                    (lambda ()
                      (interactive)
                      (browse-url url)))
        (propertize text
                    'face 'link
                    'mouse-face 'highlight
                    'help-echo url
                    'keymap map
                    'follow-link t))
    text))

(defun agent-shell-dashboard--jira-display (jiras)
  "Return display text for JIRAS."
  (if (null jiras)
      (propertize "-" 'face 'shadow)
    (mapconcat
     (lambda (jira)
       (let* ((raw-key (plist-get jira :key))
              (key (and raw-key
                        (agent-shell-dashboard--canonical-jira-key raw-key)))
              (status (plist-get jira :status))
              (resolution (plist-get jira :resolution))
              (label (concat key
                             (when status
                               (format " [%s%s]"
                                       status
                                       (if resolution
                                           (format "/%s" resolution)
                                         ""))))))
         (agent-shell-dashboard--link-text
          label
          (or (plist-get jira :url)
              (agent-shell-dashboard--jira-url key)))))
     jiras
     ", ")))

(defun agent-shell-dashboard--pr-display (prs)
  "Return display text for PRS."
  (if (null prs)
      (propertize "-" 'face 'shadow)
    (mapconcat
     (lambda (pr)
       (let ((label (format "%s/%s#%s%s"
                            (or (plist-get pr :project) "?")
                            (or (plist-get pr :repo) "?")
                            (or (plist-get pr :id) "?")
                            (if-let ((status (plist-get pr :status)))
                                (format " [%s]" status)
                              ""))))
         (agent-shell-dashboard--link-text
          label
          (let ((url (agent-shell-dashboard--pr-url pr)))
            (unless (string-prefix-p "bitbucket:" (or url ""))
              url)))))
     prs
     ", ")))

(defun agent-shell-dashboard--clean-title (title)
  "Return TITLE as a single-line non-empty string, or nil."
  (when (stringp title)
    (let ((title (string-trim
                  (replace-regexp-in-string "[ \t\n\r]+" " " title))))
      (unless (string-empty-p title)
        title))))

(defun agent-shell-dashboard--title-list (items)
  "Return cleaned titles from plist ITEMS."
  (delete-dups
   (delq nil
         (mapcar (lambda (item)
                   (agent-shell-dashboard--clean-title
                    (plist-get item :title)))
                 items))))

(defun agent-shell-dashboard--agent-buffer-title (row)
  "Return the primary live agent-shell buffer title for ROW."
  (when-let ((buffer (seq-find #'buffer-live-p (plist-get row :buffers))))
    (agent-shell-dashboard--clean-title
     (string-trim (buffer-name buffer) "\\`[ *]+" "[ *]+\\'"))))

(defun agent-shell-dashboard--branch-title (folder)
  "Return the first Git branch title below FOLDER."
  (seq-some (lambda (directory)
              (agent-shell-dashboard--clean-title
               (agent-shell-dashboard--git-branch directory)))
            (agent-shell-dashboard--git-directories folder)))

(defun agent-shell-dashboard--title-display (row)
  "Return title display for ROW."
  (let ((jira-titles (agent-shell-dashboard--title-list
                      (plist-get row :jiras)))
        (pr-titles (agent-shell-dashboard--title-list
                    (plist-get row :prs))))
    (cond
     (jira-titles
      (string-join jira-titles " | "))
     (pr-titles
      (string-join pr-titles " | "))
     ((agent-shell-dashboard--agent-buffer-title row))
     ((agent-shell-dashboard--branch-title (plist-get row :folder)))
     (t
      (propertize "<unknown>" 'face 'shadow)))))

(defun agent-shell-dashboard--truncate-left (text width)
  "Return TEXT shortened from the left to fit WIDTH columns."
  (let ((text-width (string-width text))
        (ellipsis "..."))
    (cond
     ((<= width 0)
      "")
     ((<= text-width width)
      text)
     ((<= width (string-width ellipsis))
      (truncate-string-to-width text text-width (- text-width width)))
     (t
      (let* ((available (- width (string-width ellipsis)))
             (start (- text-width available)))
        (concat ellipsis
                (truncate-string-to-width text text-width start)))))))

(defun agent-shell-dashboard--folder-display (folder)
  "Return display text for FOLDER."
  (let* ((path (abbreviate-file-name (directory-file-name folder)))
         (display (agent-shell-dashboard--truncate-left
                   path agent-shell-dashboard--folder-column-width)))
    (propertize display 'help-echo path)))

(defun agent-shell-dashboard--entry (row)
  "Return tabulated list entry for ROW."
  (let ((folder (plist-get row :folder)))
    (list (agent-shell-dashboard--row-id folder)
          (vector
           (agent-shell-dashboard--agent-icon (plist-get row :agent-state))
           (agent-shell-dashboard--folder-display folder)
           (agent-shell-dashboard--jira-display (plist-get row :jiras))
           (agent-shell-dashboard--pr-display (plist-get row :prs))
           (agent-shell-dashboard--title-display row)))))

(defun agent-shell-dashboard--print ()
  "Print dashboard entries and preserve the current row when possible."
  (let ((id (tabulated-list-get-id)))
    (tabulated-list-print)
    (agent-shell-dashboard--goto-row-id id)))

(defun agent-shell-dashboard--remove-row-id (id)
  "Remove row ID from the current dashboard table."
  (when (hash-table-p agent-shell-dashboard--rows)
    (remhash id agent-shell-dashboard--rows))
  (setq tabulated-list-entries
        (seq-remove (lambda (entry)
                      (equal (car entry) id))
                    tabulated-list-entries))
  (agent-shell-dashboard--print))

(defun agent-shell-dashboard--retain-folders (folders)
  "Drop dashboard rows whose folders are not in FOLDERS."
  (let ((valid (make-hash-table :test #'equal)))
    (unless (hash-table-p agent-shell-dashboard--rows)
      (setq agent-shell-dashboard--rows (make-hash-table :test #'equal)))
    (dolist (folder folders)
      (puthash (agent-shell-dashboard--row-id folder) t valid))
    (let (stale)
      (maphash (lambda (id _row)
                 (unless (gethash id valid)
                   (push id stale)))
               agent-shell-dashboard--rows)
      (dolist (id stale)
        (remhash id agent-shell-dashboard--rows)))
    (setq tabulated-list-entries
          (seq-filter (lambda (entry)
                        (gethash (car entry) valid))
                      tabulated-list-entries))
    (agent-shell-dashboard--print)))

(defun agent-shell-dashboard--upsert-row (row)
  "Insert or replace ROW in the current dashboard table."
  (let* ((id (agent-shell-dashboard--row-id (plist-get row :folder)))
         (entry (agent-shell-dashboard--entry row))
         (updated nil))
    (unless (hash-table-p agent-shell-dashboard--rows)
      (setq agent-shell-dashboard--rows (make-hash-table :test #'equal)))
    (puthash id row agent-shell-dashboard--rows)
    (setq tabulated-list-entries
          (mapcar (lambda (existing)
                    (if (equal (car existing) id)
                        (progn
                          (setq updated t)
                          entry)
                      existing))
                  tabulated-list-entries))
    (unless updated
      (setq tabulated-list-entries
            (sort (cons entry tabulated-list-entries)
                  (lambda (left right)
                    (string-lessp (car left) (car right))))))
    (agent-shell-dashboard--print)))

(defun agent-shell-dashboard--build-refresh-data
    (&optional full-refresh folders)
  "Return dashboard data.
When FULL-REFRESH is non-nil, also fetch Jira and PR metadata.
When FOLDERS is non-nil, build rows for those folders instead of
discovering them."
  (let ((rows (make-hash-table :test #'equal))
        (entries nil)
        (jira-cache (make-hash-table :test #'equal))
        (pr-cache (make-hash-table :test #'equal))
        (repo-pr-cache (make-hash-table :test #'equal)))
    (dolist (folder (or folders (agent-shell-dashboard--discover-folders)))
      (let ((row (agent-shell-dashboard--make-row
                  folder full-refresh jira-cache pr-cache repo-pr-cache)))
        (puthash (agent-shell-dashboard--row-id folder) row rows)
        (push (agent-shell-dashboard--entry row) entries)))
    (list :rows rows :entries (nreverse entries))))

(defun agent-shell-dashboard--apply-refresh-data (data)
  "Apply dashboard refresh DATA to the current buffer."
  (let ((rows (plist-get data :rows))
        (entries (plist-get data :entries)))
    (setq agent-shell-dashboard--rows rows
          tabulated-list-entries entries)
    (agent-shell-dashboard--print)))

(defun agent-shell-dashboard--goto-row-id (id)
  "Move point to dashboard row ID when it is still visible."
  (when id
    (ignore-errors
      (if (fboundp 'tabulated-list-goto-entry)
          (funcall (symbol-function 'tabulated-list-goto-entry) id)
        (goto-char (point-min))
        (while (and (not (eobp))
                    (not (equal (get-text-property
                                 (point) 'tabulated-list-id)
                                id)))
          (forward-line 1))))))

(defun agent-shell-dashboard--refresh (&optional full-refresh)
  "Refresh the dashboard synchronously.
When FULL-REFRESH is non-nil, also refresh Jira and PR metadata."
  (when full-refresh
    (message "Refreshing agent-shell dashboard..."))
  (agent-shell-dashboard--apply-refresh-data
   (agent-shell-dashboard--build-refresh-data full-refresh))
  (when full-refresh
    (message "Refreshing agent-shell dashboard...done")))

(defun agent-shell-dashboard--refresh-agent-status ()
  "Refresh live agent-shell status icons without fetching remote metadata."
  (when (and (derived-mode-p 'agent-shell-dashboard-mode)
             (hash-table-p agent-shell-dashboard--rows))
    (let ((changed nil))
      (maphash
       (lambda (id row)
         (let* ((folder (plist-get row :folder))
                (buffers (agent-shell-dashboard--folder-buffers folder))
                (state (agent-shell-dashboard--agent-state buffers)))
           (unless (and (equal buffers (plist-get row :buffers))
                        (eq state (plist-get row :agent-state)))
             (setq row (copy-sequence row))
             (setq row (plist-put row :buffers buffers))
             (setq row (plist-put row :agent-state state))
             (puthash id row agent-shell-dashboard--rows)
             (setq changed t))))
       agent-shell-dashboard--rows)
      (when changed
        (let ((id (tabulated-list-get-id)))
          (setq tabulated-list-entries
                (delq
                 nil
                 (mapcar
                  (lambda (entry)
                    (let* ((row (gethash (car entry)
                                          agent-shell-dashboard--rows))
                           (columns (and row
                                         (copy-sequence (cadr entry)))))
                      (when row
                        (aset columns 0
                              (agent-shell-dashboard--agent-icon
                               (plist-get row :agent-state)))
                        (list (car entry) columns))))
                  tabulated-list-entries)))
          (agent-shell-dashboard--print)
          (agent-shell-dashboard--goto-row-id id))))))

(defun agent-shell-dashboard--refresh-agent-status-buffer (buffer)
  "Refresh live agent-shell status icons in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-dashboard-mode)
        (agent-shell-dashboard--refresh-agent-status)))))

(defun agent-shell-dashboard--cancel-agent-status-timer ()
  "Cancel this dashboard buffer's live status timer."
  (when (timerp agent-shell-dashboard--agent-status-timer)
    (cancel-timer agent-shell-dashboard--agent-status-timer)
    (setq agent-shell-dashboard--agent-status-timer nil)))

(defun agent-shell-dashboard--cancel-refresh-timers ()
  "Cancel pending incremental dashboard refresh timers."
  (dolist (timer agent-shell-dashboard--refresh-timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq agent-shell-dashboard--refresh-timers nil))

(defun agent-shell-dashboard--start-agent-status-timer ()
  "Start this dashboard buffer's live status timer."
  (agent-shell-dashboard--cancel-agent-status-timer)
  (setq agent-shell-dashboard--agent-status-timer
        (run-at-time
         1 2
         #'agent-shell-dashboard--refresh-agent-status-buffer
         (current-buffer))))

(defun agent-shell-dashboard--refresh-progress (progress)
  "Advance dashboard refresh PROGRESS and update the minibuffer."
  (let ((done (1+ (or (plist-get progress :done) 0)))
        (total (plist-get progress :total)))
    (setf (plist-get progress :done) done)
    (if (< done total)
        (message "Refreshing agent-shell dashboard...%d/%d" done total)
      (setq agent-shell-dashboard--refresh-timers nil)
      (message "Refreshing agent-shell dashboard...done"))))

(defun agent-shell-dashboard--refresh-folder
    (buffer generation folder jira-cache pr-cache repo-pr-cache progress)
  "Refresh one dashboard FOLDER in BUFFER for GENERATION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'agent-shell-dashboard-mode)
                 (= generation agent-shell-dashboard--refresh-generation))
        (if (not (file-directory-p folder))
            (progn
              (agent-shell-dashboard--remove-row-id
               (agent-shell-dashboard--row-id folder))
              (agent-shell-dashboard--refresh-progress progress))
          (condition-case err
              (agent-shell-dashboard--make-row-async
               folder jira-cache pr-cache repo-pr-cache
               (lambda (row)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (and (derived-mode-p 'agent-shell-dashboard-mode)
                                (= generation agent-shell-dashboard--refresh-generation))
                       (agent-shell-dashboard--upsert-row row)
                       (agent-shell-dashboard--refresh-progress progress))))))
            (error
             (message "Refreshing agent-shell dashboard row failed for %s: %s"
                      (abbreviate-file-name folder)
                      (error-message-string err))
             (agent-shell-dashboard--refresh-progress progress))))))))

(defun agent-shell-dashboard--refresh-incremental ()
  "Refresh the dashboard incrementally with one idle timer per folder."
  (let* ((buffer (current-buffer))
         (folders (agent-shell-dashboard--discover-folders))
         (total (length folders))
         (generation (cl-incf agent-shell-dashboard--refresh-generation))
         (jira-cache (make-hash-table :test #'equal))
         (pr-cache (make-hash-table :test #'equal))
         (repo-pr-cache (make-hash-table :test #'equal))
         (progress (list :done 0 :total total)))
    (agent-shell-dashboard--cancel-refresh-timers)
    (agent-shell-dashboard--retain-folders folders)
    (if (zerop total)
        (message "Refreshing agent-shell dashboard...done")
      (message "Refreshing agent-shell dashboard...scheduled %d rows" total)
      (setq agent-shell-dashboard--refresh-timers
            (cl-loop
             for folder in folders
             for index from 1
             collect
             (run-with-idle-timer
              (+ agent-shell-dashboard-refresh-idle-delay
                 (* (1- index)
                    agent-shell-dashboard-refresh-idle-step))
              nil
              #'agent-shell-dashboard--refresh-folder
              buffer generation folder
              jira-cache pr-cache repo-pr-cache
              progress))))))

;;;###autoload
(defun agent-shell-dashboard-refresh ()
  "Refresh live agent-shell, Jira, and PR status incrementally."
  (interactive)
  (unless (derived-mode-p 'agent-shell-dashboard-mode)
    (user-error "Not in an agent-shell dashboard buffer"))
  (agent-shell-dashboard--ensure-mode-map)
  (agent-shell-dashboard--refresh-incremental))

(defun agent-shell-dashboard--row-at-point ()
  "Return dashboard row at point."
  (let ((id (tabulated-list-get-id)))
    (or (and id (gethash id agent-shell-dashboard--rows))
        (user-error "No dashboard row at point"))))

(defun agent-shell-dashboard--row-buffer (row)
  "Return preferred live agent-shell buffer for ROW, or nil."
  (car (plist-get row :buffers)))

;;;###autoload
(defun agent-shell-dashboard-visit ()
  "Visit or launch the agent-shell for the current row."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (folder (plist-get row :folder)))
    (if-let ((buffer (agent-shell-dashboard--row-buffer row)))
        (pop-to-buffer buffer)
      (let* ((launch-directory
              (agent-shell-dashboard--launch-directory folder t))
             (default-directory launch-directory)
             (agent-shell-session-strategy 'prompt)
             (agent-shell-cwd-function (lambda () launch-directory))
             (config (or (and (fboundp 'agent-shell--resolve-preferred-config)
                              (agent-shell--resolve-preferred-config))
                         (agent-shell-select-config
                          :prompt "Start new agent: "))))
        (agent-shell--start :config config
                            :session-strategy 'prompt)))))

(defun agent-shell-dashboard-rename-buffer ()
  "Rename the live agent-shell buffer on the current row."
  (interactive)
  (if-let ((buffer (agent-shell-dashboard--row-buffer
                    (agent-shell-dashboard--row-at-point))))
      (progn
        (with-current-buffer buffer
          (call-interactively #'agent-shell-rename-buffer))
        (agent-shell-dashboard--refresh nil))
    (user-error "No live agent-shell buffer on this row")))

(defun agent-shell-dashboard--read-jira (row)
  "Read a Jira entry from ROW."
  (let ((jiras (plist-get row :jiras)))
    (pcase jiras
      ('nil (user-error "No Jira issue on this row"))
      (`(,only) only)
      (_ (let* ((choices (mapcar (lambda (jira)
                                   (cons (format "%s %s"
                                                 (plist-get jira :key)
                                                 (or (plist-get jira :title)
                                                     ""))
                                         jira))
                                 jiras))
                (choice (completing-read "Jira: " choices nil t)))
           (cdr (assoc choice choices)))))))

(defun agent-shell-dashboard--read-pr (row)
  "Read a PR entry from ROW."
  (let ((prs (plist-get row :prs)))
    (pcase prs
      ('nil (user-error "No PR on this row"))
      (`(,only) only)
      (_ (let* ((choices (mapcar (lambda (pr)
                                   (cons (format "%s %s"
                                                 (agent-shell-dashboard--pr-key pr)
                                                 (or (plist-get pr :title)
                                                     ""))
                                         pr))
                                 prs))
                (choice (completing-read "PR: " choices nil t)))
           (cdr (assoc choice choices)))))))

(defun agent-shell-dashboard-open-jira ()
  "Open the Jira issue on the current row."
  (interactive)
  (let* ((jira (agent-shell-dashboard--read-jira
                (agent-shell-dashboard--row-at-point)))
         (url (or (plist-get jira :url)
                  (agent-shell-dashboard--jira-url (plist-get jira :key)))))
    (unless url
      (user-error "No Jira URL known for %s" (plist-get jira :key)))
    (browse-url url)))

(defun agent-shell-dashboard-open-pr ()
  "Open the PR on the current row."
  (interactive)
  (let* ((pr (agent-shell-dashboard--read-pr
              (agent-shell-dashboard--row-at-point)))
         (url (agent-shell-dashboard--pr-url pr)))
    (unless (and url (not (string-prefix-p "bitbucket:" url)))
      (user-error "No browser URL known for %s"
                  (agent-shell-dashboard--pr-key pr)))
    (browse-url url)))

(defun agent-shell-dashboard--choose-link-kind (row prompt)
  "Return `jira' or `pr' for ROW using PROMPT when both exist."
  (let ((has-jira (plist-get row :jiras))
        (has-pr (plist-get row :prs)))
    (cond
     ((and has-jira has-pr)
      (cdr (assoc
            (completing-read prompt '(("Jira issue" . jira)
                                      ("Bitbucket PR" . pr))
                             nil t)
            '(("Jira issue" . jira)
              ("Bitbucket PR" . pr)))))
     (has-jira 'jira)
     (has-pr 'pr)
     (t (user-error "No Jira issue or PR on this row")))))

(defun agent-shell-dashboard-open ()
  "Open the Jira issue or PR on the current row."
  (interactive)
  (pcase (agent-shell-dashboard--choose-link-kind
          (agent-shell-dashboard--row-at-point)
          "Open: ")
    ('jira (agent-shell-dashboard-open-jira))
    ('pr (agent-shell-dashboard-open-pr))))

(defun agent-shell-dashboard-close-or-decline ()
  "Close the Jira issue or decline the PR on the current row."
  (interactive)
  (pcase (agent-shell-dashboard--choose-link-kind
          (agent-shell-dashboard--row-at-point)
          "Close/decline: ")
    ('jira (agent-shell-dashboard-close-jira))
    ('pr (agent-shell-dashboard-decline-pr))))

(defun agent-shell-dashboard-find-file ()
  "Run `find-file' starting in the current row's directory."
  (interactive)
  (let* ((directory (plist-get (agent-shell-dashboard--row-at-point) :folder))
         (default-directory (agent-shell-dashboard--directory directory)))
    (find-file (read-file-name "Find file: " default-directory))))

(defun agent-shell-dashboard--jira-transitions (key)
  "Return available Jira transitions for KEY using jira.el."
  (when (require 'jira-api nil t)
    (let* ((response (jira-api-call
                      "GET" (format "issue/%s/transitions" key)
                      :sync t))
           (data (request-response-data response)))
      (mapcar
       (lambda (transition)
         (let ((to (map-elt transition 'to)))
           (list :id (map-elt transition 'id)
                 :name (map-elt transition 'name)
                 :to (map-elt to 'name))))
       (map-elt data 'transitions)))))

(defun agent-shell-dashboard--transition-jira-via-api (key target transitions)
  "Transition Jira KEY to TARGET using TRANSITIONS and jira.el."
  (when-let* ((transition
               (seq-find
                (lambda (candidate)
                  (or (string-equal target (plist-get candidate :name))
                      (string-equal target (plist-get candidate :to))))
                transitions))
              (id (plist-get transition :id)))
    (jira-api-call
     "POST" (format "issue/%s/transitions" key)
     :data `(("transition" . (("id" . ,id))))
     :parser (lambda () nil)
     :sync t)
    t))

(defun agent-shell-dashboard--transition-jira-via-gdev (key target)
  "Transition Jira KEY to TARGET using gdev-cli."
  (agent-shell-dashboard--run-string-sync
   "gdev-cli" "jira" "transition" "--force"
   (concat "--issues=" key)
   (concat "--targetStatus=" target)))

(defun agent-shell-dashboard--split-comma-list (text)
  "Split comma-separated TEXT into trimmed non-empty strings."
  (seq-filter
   (lambda (item)
     (not (string-empty-p item)))
   (mapcar #'string-trim
           (split-string (or text "") ","))))

(defun agent-shell-dashboard--jira-close-update-data (fix-version labels)
  "Return Jira update payload for FIX-VERSION and LABELS."
  (let (data)
    (unless (string-empty-p fix-version)
      (push (cons "fields"
                  `(("fixVersions" . ,(vector `(("name" . ,fix-version))))))
            data))
    (when labels
      (push (cons "update"
                  `(("labels" . ,(vconcat
                                   (mapcar (lambda (label)
                                             `(("add" . ,label)))
                                           labels)))))
            data))
    (nreverse data)))

(defun agent-shell-dashboard--update-jira-close-fields-via-api
    (key fix-version labels)
  "Update Jira KEY with FIX-VERSION and LABELS using jira.el."
  (when-let ((data (agent-shell-dashboard--jira-close-update-data
                    fix-version labels)))
    (jira-api-call
     "PUT" (format "issue/%s" key)
     :data data
     :parser (lambda () nil)
     :sync t)
    t))

(defun agent-shell-dashboard--update-jira-close-fields-via-gdev
    (key fix-version labels)
  "Update Jira KEY with FIX-VERSION and LABELS using gdev-cli."
  (when-let ((data (agent-shell-dashboard--jira-close-update-data
                    fix-version labels)))
    (let ((template (make-temp-file "agent-shell-dashboard-jira-" nil ".json")))
      (unwind-protect
          (progn
            (with-temp-file template
              (insert (json-encode data)))
            (agent-shell-dashboard--run-string-sync
             "gdev-cli" "jira" "update"
             (concat "-id=" key)
             (concat "-template=" template)))
        (when (file-exists-p template)
          (delete-file template))))
    t))

(defun agent-shell-dashboard--update-jira-close-fields
    (key fix-version labels)
  "Update Jira KEY with FIX-VERSION and LABELS when either is present."
  (if (not (or (not (string-empty-p fix-version)) labels))
      t
    (or (condition-case nil
            (and (require 'jira-api nil t)
                 (agent-shell-dashboard--update-jira-close-fields-via-api
                  key fix-version labels))
          (error nil))
        (agent-shell-dashboard--update-jira-close-fields-via-gdev
         key fix-version labels))))

(defun agent-shell-dashboard--jira-close-confirmation
    (key target fix-version labels)
  "Return confirmation prompt for closing KEY to TARGET."
  (format "Transition %s to %s%s%s? "
          key
          target
          (if (string-empty-p fix-version)
              ""
            (format ", set fixVersion %s" fix-version))
          (if labels
              (format ", add labels %s" (string-join labels ", "))
            "")))

(defun agent-shell-dashboard-close-jira ()
  "Close or otherwise transition the Jira issue on the current row."
  (interactive)
  (let* ((jira (agent-shell-dashboard--read-jira
                (agent-shell-dashboard--row-at-point)))
         (key (plist-get jira :key))
         (transitions (condition-case nil
                          (agent-shell-dashboard--jira-transitions key)
                        (error nil)))
         (choices (delete-dups
                   (seq-filter
                    #'identity
                    (append (mapcar (lambda (transition)
                                      (plist-get transition :to))
                                    transitions)
                            (mapcar (lambda (transition)
                                      (plist-get transition :name))
                                    transitions)
                            '("Closed" "Done" "Resolved")))))
         (target (completing-read
                  (format "Transition %s to: " key)
                  choices nil nil nil nil
                  (or (seq-find (lambda (choice)
                                  (member choice choices))
                                '("Closed" "Done" "Resolved"))
                      (car choices))))
         (fix-version (string-trim
                       (read-string
                        (format "Fix version for %s (empty to leave unchanged): "
                                key))))
         (labels (agent-shell-dashboard--split-comma-list
                  (read-string
                   (format "Additional labels for %s (comma-separated): "
                           key)))))
    (when (yes-or-no-p
           (agent-shell-dashboard--jira-close-confirmation
            key target fix-version labels))
      (condition-case err
          (progn
            (agent-shell-dashboard--update-jira-close-fields
             key fix-version labels)
            (unless (and transitions
                         (agent-shell-dashboard--transition-jira-via-api
                          key target transitions))
              (agent-shell-dashboard--transition-jira-via-gdev key target)))
        (error
         (user-error "Could not transition %s: %s"
                     key (error-message-string err))))
      (agent-shell-dashboard-refresh))))

(defun agent-shell-dashboard-decline-pr ()
  "Decline the PR on the current row."
  (interactive)
  (let* ((pr (agent-shell-dashboard--read-pr
              (agent-shell-dashboard--row-at-point)))
         (project (or (plist-get pr :project)
                      (user-error "PR is missing project")))
         (repo (or (plist-get pr :repo)
                   (user-error "PR is missing repo")))
         (id (or (plist-get pr :id)
                 (user-error "PR is missing id"))))
    (when (yes-or-no-p (format "Decline PR %s/%s#%s? " project repo id))
      (agent-shell-dashboard--run-string-sync
       "gdev-cli" "bitbucket" "decline"
       (concat "--project=" project)
       (concat "--repo=" repo)
       (concat "--pull-requests=" id))
      (agent-shell-dashboard-refresh))))

(defun agent-shell-dashboard--linked-git-worktree-p (directory)
  "Return non-nil when DIRECTORY has a linked-worktree `.git' file."
  (let ((dotgit (expand-file-name ".git" directory)))
    (and (file-regular-p dotgit)
         (with-temp-buffer
           (insert-file-contents dotgit nil 0 4096)
           (goto-char (point-min))
           (looking-at "gitdir:[ \t]+")))))

(defun agent-shell-dashboard--git-common-dir (directory)
  "Return Git common dir for linked worktree DIRECTORY."
  (when-let ((common (agent-shell-dashboard--git-output
                      directory "rev-parse" "--git-common-dir")))
    (if (file-name-absolute-p common)
        common
      (expand-file-name common directory))))

(defun agent-shell-dashboard--remove-git-worktree-link (directory)
  "Remove Git worktree metadata for linked worktree DIRECTORY."
  (when (agent-shell-dashboard--linked-git-worktree-p directory)
    (if-let ((common-dir (agent-shell-dashboard--git-common-dir directory)))
        (let ((default-directory
               (or (agent-shell-dashboard--parent-directory directory)
                   default-directory)))
          (with-temp-buffer
            (let ((exit (process-file
                         "git" nil t nil
                         (concat "--git-dir=" common-dir)
                         "worktree" "remove" "--force"
                         (agent-shell-dashboard--directory directory))))
              (unless (zerop exit)
                (unless (yes-or-no-p
                         (format "git worktree remove failed for %s. Delete anyway? "
                                 directory))
                  (error "%s" (string-trim (buffer-string))))))))
      (unless (yes-or-no-p
               (format "Could not determine common Git dir for %s. Delete anyway? "
                       directory))
        (user-error "Could not determine common Git dir for %s" directory))
      (message "Could not determine common Git dir for %s; deleting anyway"
               directory))))

(defun agent-shell-dashboard-delete-worktree ()
  "Remove Git worktree links and delete the current agent-shell worktree folder."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (folder (plist-get row :folder)))
    (unless (agent-shell-dashboard--agent-worktree-folder-p folder)
      (user-error "Refusing to delete non-agent-shell worktree folder: %s"
                  folder))
    (when (yes-or-no-p
           (format "Delete %s, including one-level Git worktree links? "
                   (abbreviate-file-name folder)))
      (dolist (buffer (plist-get row :buffers))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (dolist (directory (agent-shell-dashboard--git-directories folder))
        (agent-shell-dashboard--remove-git-worktree-link directory))
      (when (file-directory-p folder)
        (delete-directory folder t))
      (agent-shell-dashboard--remove-row-id
       (agent-shell-dashboard--row-id folder))
      (agent-shell-dashboard--refresh nil)
      (message "Deleted %s" (abbreviate-file-name folder)))))

(transient-define-prefix agent-shell-dashboard-dispatch ()
  "Show dashboard actions."
  [["Agent"
    ("RET" "visit/start" agent-shell-dashboard-visit)
    ("R" "rename buffer" agent-shell-dashboard-rename-buffer)
    ("g" "refresh" agent-shell-dashboard-refresh)]
   ["Links"
    ("o" "open Jira/PR" agent-shell-dashboard-open)
    ("j" "open Jira" agent-shell-dashboard-open-jira)
    ("p" "open PR" agent-shell-dashboard-open-pr)]
   ["Remote"
    ("c" "close/decline" agent-shell-dashboard-close-or-decline)
    ("J" "close Jira" agent-shell-dashboard-close-jira)
    ("P" "decline PR" agent-shell-dashboard-decline-pr)]
   ["Local"
    ("f" "find file here" agent-shell-dashboard-find-file)
    ("D" "delete worktree" agent-shell-dashboard-delete-worktree)]])

(defvar agent-shell-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'agent-shell-dashboard-visit)
    (define-key map (kbd "R") #'agent-shell-dashboard-rename-buffer)
    (define-key map (kbd "g") #'agent-shell-dashboard-refresh)
    (define-key map (kbd "o") #'agent-shell-dashboard-open)
    (define-key map (kbd "c") #'agent-shell-dashboard-close-or-decline)
    (define-key map (kbd "f") #'agent-shell-dashboard-find-file)
    (define-key map (kbd "C-x C-f") #'agent-shell-dashboard-find-file)
    (define-key map (kbd "D") #'agent-shell-dashboard-delete-worktree)
    (define-key map (kbd "?") #'agent-shell-dashboard-dispatch)
    map)
  "Keymap for `agent-shell-dashboard-mode'.")

(defun agent-shell-dashboard--ensure-mode-map ()
  "Install the current dashboard mode map in this buffer."
  (unless (eq (current-local-map) agent-shell-dashboard-mode-map)
    (use-local-map agent-shell-dashboard-mode-map)))

(define-derived-mode agent-shell-dashboard-mode tabulated-list-mode "Agent-Shell-Dashboard"
  "Major mode for the agent-shell worktree dashboard."
  (agent-shell-dashboard--ensure-mode-map)
  (setq tabulated-list-format
        (vector
         (list "A" 2 t)
         (list "Folder" agent-shell-dashboard--folder-column-width t)
         (list "Jira" agent-shell-dashboard--jira-column-width t)
         (list "PR" 34 t)
         (list "Title" 0 t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq-local header-line-format
              "RET: visit/start  g: refresh  o: open  c: close/decline  C-x C-f: find file  D: delete worktree  ?: menu")
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (agent-shell-dashboard--refresh-incremental)))
  (add-hook 'kill-buffer-hook
            #'agent-shell-dashboard--cancel-agent-status-timer nil t)
  (add-hook 'kill-buffer-hook
            #'agent-shell-dashboard--cancel-refresh-timers nil t)
  (agent-shell-dashboard--start-agent-status-timer)
  (tabulated-list-init-header))

;;;###autoload
(defun agent-shell-dashboard ()
  "Open the agent-shell worktree dashboard."
  (interactive)
  (let ((buffer (get-buffer-create agent-shell-dashboard--buffer-name)))
    (with-current-buffer buffer
      (agent-shell-dashboard-mode)
      (agent-shell-dashboard--ensure-mode-map)
      (agent-shell-dashboard--refresh nil))
    (pop-to-buffer buffer)))

(provide 'agent-shell-dashboard)

;;; agent-shell-dashboard.el ends here
