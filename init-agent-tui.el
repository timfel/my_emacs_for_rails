;;; -*- lexical-binding: t -*-

(require 'use-package)

(use-package agent-tui
  :vc (:url "https://github.com/timfel/agent-tui.git"
            :branch "main"
            :rev :newest)
  :demand t
  :config
  ;; Provider packages bind this dynamically too; keeping the provider global
  ;; makes fan-out and dashboard commands know which TUI to launch.
  (fset 'timfel-agent-start #'agent-tui-pi-start)
  (setq agent-tui-provider 'pi)
  (add-hook 'agent-tui-idle-hook
            (lambda ()
              (when (bound-and-true-p show-help-function)
                (funcall show-help-function
                         (concat "Your agent in " default-directory
                                 " is ready for you now. "
                                 (format-time-string "%F %T")
                                 " [" (buffer-name) "]"))))))

(use-package agent-tui-pi
  :after agent-tui
  :commands agent-tui-pi-start
  :bind (("C-x a s" . agent-tui-pi-start)))

(use-package agent-tui-bookmark
  :after agent-tui
  :config
  (require 'agent-tui-ol))

(use-package agent-tui-desktop
  :after agent-tui
  :config
  (agent-tui-desktop-mode 1)
  (add-hook 'agent-tui-started-hook
            (lambda ()
              (when (and desktop-save-mode desktop-dirname desktop-file-modtime)
                (run-with-idle-timer 5 nil #'desktop-save desktop-dirname nil t))))
  (add-hook 'desktop-save-mode-hook
            (lambda ()
              (agent-tui-desktop-mode (if desktop-save-mode 1 -1)))))

(use-package agent-tui-bwrap
  :after agent-tui
  :functions agent-tui-bwrap-mode
  :custom
  (agent-tui-bwrap-cpu-limit 8)
  (agent-tui-bwrap-memory-limit-gb 48)
  :config
  (agent-tui-bwrap-mode 1))

(use-package agent-tui-context
  :after agent-tui
  :commands agent-tui-enqueue-context-prompt
  :bind (("C-x a c" . agent-tui-enqueue-context-prompt)))

(use-package agent-tui-fanout
  :after agent-tui
  :functions (agent-tui-fanout-default-repositories
              timfel/agent-tui-fanout-graal-repositories)
  :custom
  (agent-tui-fanout-provider 'pi)
  (agent-tui-fanout-adjacent-repository-names
   '("graal" "graal-enterprise"))
  :config
  (defun timfel/agent-tui-fanout-graal-repositories (repo-root)
    "Return fan-out repositories for REPO-ROOT, preserving Graal suite siblings."
    (let* ((repo-root (file-name-as-directory (expand-file-name repo-root)))
           (repo-name (file-name-nondirectory (directory-file-name repo-root)))
           (mx-dir (expand-file-name (format "mx.%s" repo-name) repo-root))
           (agent-tui-fanout-adjacent-repository-names
            (when (file-directory-p mx-dir)
              agent-tui-fanout-adjacent-repository-names)))
      (agent-tui-fanout-default-repositories repo-root)))
  (setq agent-tui-fanout-repositories-function
        #'timfel/agent-tui-fanout-graal-repositories))

(use-package agent-tui-jira
  :after (agent-tui agent-tui-fanout)
  :defines (jira-detail-mode-map jira-issues-mode-map)
  :commands agent-tui-jira-investigate-marked
  :bind (:map jira-detail-mode-map
         ("C-x a i" . agent-tui-jira-investigate-marked)
         :map jira-issues-mode-map
         ("C-x a i" . agent-tui-jira-investigate-marked)))

(use-package agent-tui-dashboard
  :after (agent-tui agent-tui-fanout)
  :custom
  (agent-tui-dashboard-provider 'pi)
  (agent-tui-dashboard-search-directories
   (list user-emacs-directory "~/dev/graalpython" "~/dev/graal"
         "~/dev/graalos" "~/tmp" "~/tmp/workloads"
         "~/tmp/graalos-apps/"))
  :bind (("C-x a a" . agent-tui-dashboard)))

;;; init-agent-tui.el ends here
