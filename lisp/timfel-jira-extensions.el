;;; timfel-jira-extensions.el --- Jira helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Local helpers around `jira.el' for querying and opening Jira issues.

;;; Code:

(require 'timfel)
(require 'jira)

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
