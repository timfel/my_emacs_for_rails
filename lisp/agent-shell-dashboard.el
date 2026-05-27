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

(defconst agent-shell-dashboard--buffer-name "*agent-shell dashboard*")

(defconst agent-shell-dashboard--pr-regexp
  "\\(?:https?://[^ \t\n\r)>\"]+\\|bitbucket:\\)?/?projects/\\([^/ \t\n\r)>\"]+\\)/repos/\\([^/ \t\n\r)>\"]+\\)/pull-requests/\\([0-9]+\\)")

(defvar-local agent-shell-dashboard--rows nil
  "Hash table mapping row ids to row plists in the current dashboard.")

(defvar agent-shell-cwd-function)
(defvar agent-shell-session-strategy)
(defvar agent-shell--state)
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

(defun agent-shell-dashboard--agent-state (buffers)
  "Return aggregate agent state for BUFFERS."
  (cond
   ((seq-some #'agent-shell-dashboard--permission-pending-p buffers)
    'permission)
   ((seq-some #'agent-shell-dashboard--buffer-busy-p buffers)
    'busy)
   (buffers
    'idle)
   (t
    'none)))

(defun agent-shell-dashboard--agent-icon (state)
  "Return a propertized icon for aggregate STATE."
  (pcase state
    ('permission
     (propertize "!" 'face 'warning 'help-echo "agent-shell is waiting for permission"))
    ('busy
     (propertize "*" 'face 'font-lock-keyword-face 'help-echo "agent-shell is busy"))
    ('idle
     (propertize "." 'face 'success 'help-echo "agent-shell is idle"))
    (_
     (propertize "-" 'face 'shadow 'help-echo "no live agent-shell"))))

(defun agent-shell-dashboard--run-string (program &rest args)
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

(defun agent-shell-dashboard--run-string-or-nil (program &rest args)
  "Run PROGRAM with ARGS and return stdout, or nil on failure."
  (condition-case nil
      (string-trim (apply #'agent-shell-dashboard--run-string program args))
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

(defun agent-shell-dashboard--run-json (program &rest args)
  "Run PROGRAM with ARGS and parse JSON output."
  (agent-shell-dashboard--json-from-string
   (apply #'agent-shell-dashboard--run-string program args)))

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

(defun agent-shell-dashboard--git-remote-coordinates (directory)
  "Infer Bitbucket project/repo coordinates from DIRECTORY's origin URL."
  (when-let ((remote (agent-shell-dashboard--git-remote directory)))
    (cond
     ((string-match "/projects/\\([^/]+\\)/repos/\\([^/.]+\\)" remote)
      (list :project (match-string 1 remote)
            :repo (match-string 2 remote)))
     ((string-match "/scm/\\([^/]+\\)/\\([^/.]+\\)\\(?:\\.git\\)?\\'" remote)
      (list :project (upcase (match-string 1 remote))
            :repo (match-string 2 remote)))
     ((string-match "[:/]\\([^/:]+\\)/\\([^/.]+\\)\\(?:\\.git\\)?\\'" remote)
      (list :project (upcase (match-string 1 remote))
            :repo (match-string 2 remote))))))

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
    (let ((data (agent-shell-dashboard--run-json
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
       (consp (car object))))

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
             (data (agent-shell-dashboard--run-json
                    "gdev-cli" "bitbucket" "list-prs"
                    "--json"
                    (concat "--project=" project)
                    (concat "--repos=" repo)
                    "--state=ALL")))
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
            (fetched (agent-shell-dashboard--cache-get-or-put
                      cache key
                      (lambda () (agent-shell-dashboard--fetch-pr pr)))))
       (append fetched pr)))
   prs))

(defun agent-shell-dashboard--make-row (folder &optional full-refresh jira-cache pr-cache)
  "Build row plist for FOLDER.
When FULL-REFRESH is non-nil, refresh Jira and PR metadata using CACHES."
  (let* ((buffers (agent-shell-dashboard--folder-buffers folder))
         (state (agent-shell-dashboard--agent-state buffers))
         (links (agent-shell-dashboard--extract-local-links folder))
         (jiras (plist-get links :jiras))
         (prs (plist-get links :prs)))
    (when full-refresh
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

(defun agent-shell-dashboard--folder-display (folder)
  "Return display text for FOLDER."
  (abbreviate-file-name (directory-file-name folder)))

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

(defun agent-shell-dashboard--refresh (&optional full-refresh)
  "Refresh the dashboard.
When FULL-REFRESH is non-nil, also refresh Jira and PR metadata."
  (let ((rows (make-hash-table :test #'equal))
        (entries nil)
        (jira-cache (make-hash-table :test #'equal))
        (pr-cache (make-hash-table :test #'equal)))
    (when full-refresh
      (message "Refreshing agent-shell dashboard..."))
    (dolist (folder (agent-shell-dashboard--discover-folders))
      (let ((row (agent-shell-dashboard--make-row
                  folder full-refresh jira-cache pr-cache)))
        (puthash (agent-shell-dashboard--row-id folder) row rows)
        (push (agent-shell-dashboard--entry row) entries)))
    (setq agent-shell-dashboard--rows rows
          tabulated-list-entries (nreverse entries))
    (tabulated-list-print t)
    (when full-refresh
      (message "Refreshing agent-shell dashboard...done"))))

;;;###autoload
(defun agent-shell-dashboard-refresh ()
  "Refresh live agent-shell, Jira, and PR status in the dashboard."
  (interactive)
  (unless (derived-mode-p 'agent-shell-dashboard-mode)
    (user-error "Not in an agent-shell dashboard buffer"))
  (agent-shell-dashboard--refresh t))

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
  (agent-shell-dashboard--run-string
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
            (agent-shell-dashboard--run-string
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
      (agent-shell-dashboard--run-string
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
    (let* ((common-dir (or (agent-shell-dashboard--git-common-dir directory)
                           (error "Could not determine common Git dir for %s"
                                  directory)))
           (default-directory
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
              (error "%s" (string-trim (buffer-string))))))))))

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
      (agent-shell-dashboard--refresh nil)
      (message "Deleted %s" (abbreviate-file-name folder)))))

(transient-define-prefix agent-shell-dashboard-dispatch ()
  "Show dashboard actions."
  [["Agent"
    ("RET" "visit/start" agent-shell-dashboard-visit)
    ("g" "refresh" agent-shell-dashboard-refresh)]
   ["Links"
    ("j" "open Jira" agent-shell-dashboard-open-jira)
    ("p" "open PR" agent-shell-dashboard-open-pr)]
   ["Remote"
    ("c" "close Jira" agent-shell-dashboard-close-jira)
    ("d" "decline PR" agent-shell-dashboard-decline-pr)]
   ["Local"
    ("D" "delete worktree" agent-shell-dashboard-delete-worktree)]])

(defvar agent-shell-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'agent-shell-dashboard-visit)
    (define-key map (kbd "g") #'agent-shell-dashboard-refresh)
    (define-key map (kbd "j") #'agent-shell-dashboard-open-jira)
    (define-key map (kbd "p") #'agent-shell-dashboard-open-pr)
    (define-key map (kbd "c") #'agent-shell-dashboard-close-jira)
    (define-key map (kbd "d") #'agent-shell-dashboard-decline-pr)
    (define-key map (kbd "D") #'agent-shell-dashboard-delete-worktree)
    (define-key map (kbd ".") #'agent-shell-dashboard-dispatch)
    map)
  "Keymap for `agent-shell-dashboard-mode'.")

(define-derived-mode agent-shell-dashboard-mode tabulated-list-mode "Agent-Shell-Dashboard"
  "Major mode for the agent-shell worktree dashboard."
  (setq tabulated-list-format
        [("A" 2 t)
         ("Folder" 42 t)
         ("Jira" 30 t)
         ("PR" 34 t)
         ("Title" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq-local header-line-format
              "RET: visit/start  g: refresh  j/p: open links  c: close Jira  d: decline PR  D: delete worktree  .: menu")
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (agent-shell-dashboard--refresh t)))
  (tabulated-list-init-header))

;;;###autoload
(defun agent-shell-dashboard ()
  "Open the agent-shell worktree dashboard."
  (interactive)
  (let ((buffer (get-buffer-create agent-shell-dashboard--buffer-name)))
    (with-current-buffer buffer
      (agent-shell-dashboard-mode)
      (agent-shell-dashboard--refresh nil))
    (pop-to-buffer buffer)))

(provide 'agent-shell-dashboard)

;;; agent-shell-dashboard.el ends here
