;;; timfel-jira-extensions.el --- Jira helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Local helpers around `jira.el' for querying and opening Jira issues.

;;; Code:

(require 'tabulated-list)

(require 'timfel)
(require 'jira)
(require 'orcl (expand-file-name "orcl.el" timfel/gist-location))

(defvar timfel/jira-periodic-issues-buffer-name "*Jira Periodic Issues*"
  "Buffer name for periodic Jira issues.")

(defvar-local timfel/jira-periodic-issues--days 90
  "Number of days used to populate the current periodic issues buffer.")

(defun timfel/jira-periodic-issues-alist (&optional days)
  "Return an alist of recent periodic Jira issues within DAYS.

This currently delegates to `timfel/jira-periodic-python-issues-alist'."
  (timfel/jira-periodic-python-issues-alist days))

;;;###autoload
(defun timfel/jira-periodic-python-issues-alist (&optional days)
  "Return an alist of recent periodic Python Jira issues.

The return value is a list of `(KEY . SUMMARY)' pairs for issues with label
`periodic-job-failures', component `Python', status other than `Closed' or
`In Progress', and created within DAYS days.  DAYS defaults to 90."
  (let* ((days (or days 90))
         (jql (concat
               "labels = periodic-job-failures "
               "AND component = \"Python\" "
               "AND status NOT IN (\"Closed\", \"In Progress\") "
               (format "AND created >= -%dd " days)
               "ORDER BY created DESC")))
    (unless (require 'jira nil t)
      (user-error "jira.el is not installed"))
    (unless (require 'jira-api nil t)
      (user-error "jira-api.el is not available"))
    (let* ((response
            (jira-api-search
             :sync t
             :params `(("jql" . ,jql)
                       ("maxResults" . "100")
                       ("fields" . "key,summary"))))
           (data (request-response-data response))
           (issues (append (alist-get 'issues data) nil)))
      (mapcar (lambda (issue)
                (cons (alist-get 'key issue)
                      (alist-get 'summary (alist-get 'fields issue))))
              issues))))

(defun timfel/jira-periodic-issues--issue-at-point ()
  "Return the Jira issue key for the current tabulated row."
  (or (tabulated-list-get-id)
      (user-error "No Jira issue on this line")))

(defun timfel/jira-periodic-issues-open-issue ()
  "Open the Jira issue at point inside Emacs."
  (interactive)
  (jira-detail-show-issue (timfel/jira-periodic-issues--issue-at-point)))

(defun timfel/jira-periodic-issues-open-issue-in-browser ()
  "Open the Jira issue at point in an external browser."
  (interactive)
  (jira-actions-open-issue (timfel/jira-periodic-issues--issue-at-point)))

(defun timfel/jira-periodic-issues--refresh (&optional days)
  "Populate the current buffer with periodic Jira issues for DAYS."
  (let* ((days (or days timfel/jira-periodic-issues--days 90))
         (issues (timfel/jira-periodic-issues-alist days)))
    (setq-local timfel/jira-periodic-issues--days days)
    (setq tabulated-list-entries
          (mapcar (lambda (issue)
                    (let ((key (car issue))
                          (summary (cdr issue)))
                      (list key (vector key (or summary "")))))
                  issues))))

(defun timfel/jira-periodic-issues--revert (_ignore-auto _noconfirm)
  "Refresh the periodic Jira issues buffer."
  (timfel/jira-periodic-issues--refresh)
  (tabulated-list-print t))

(defvar timfel/jira-periodic-issues-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "I") #'timfel/jira-periodic-issues-open-issue)
    (define-key map (kbd "O") #'timfel/jira-periodic-issues-open-issue-in-browser)
    map)
  "Keymap for `timfel/jira-periodic-issues-mode'.")

(define-derived-mode timfel/jira-periodic-issues-mode tabulated-list-mode "Jira-Periodic"
  "Major mode for viewing periodic Jira issues."
  (setq tabulated-list-format [("Issue" 18 t)
                               ("Summary" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Issue" nil))
  (setq-local header-line-format
              "I: open issue in Jira    O: open issue in browser")
  (setq-local revert-buffer-function #'timfel/jira-periodic-issues--revert)
  (tabulated-list-init-header))

;;;###autoload
(defun timfel/jira-periodic-issues (&optional days)
  "Show recent periodic Jira issues from the last DAYS days.

With a prefix argument, prompt for DAYS.  DAYS defaults to 90."
  (interactive
   (list (when current-prefix-arg
           (read-number "Show periodic Jira issues from last N days: " 90))))
  (let ((buffer (get-buffer-create timfel/jira-periodic-issues-buffer-name)))
    (with-current-buffer buffer
      (timfel/jira-periodic-issues-mode)
      (timfel/jira-periodic-issues--refresh days)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

;;;###autoload
(defun timfel/jira ()
  "Open the Jira issues UI after priming cached Jira metadata."
  (interactive)
  (unless (require 'jira nil t)
    (user-error "jira.el is not installed"))
  (condition-case nil
      (jira-api-get-basic-data)
    (error nil))
  (jira-api-get-users)
  (jira-api-get-fields)
  (jira-api-get-statuses)
  (jira-api-get-resolutions)
  (jira-api-get-projects)
  (funcall-interactively #'jira-issues))

(provide 'timfel-jira-extensions)

;;; timfel-jira-extensions.el ends here
