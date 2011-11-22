(defun gtd ()
  (interactive)
  (el-get-init "slack-rtm")
  (if (eq nil (get-buffer "*todo*"))
      (slack-rtm slack-rtm-default-query)
    (switch-to-buffer-other-window "*todo*")))

(global-set-key "\C-x\C-g" 'gtd)

(add-hook 'org-mode-hook (lambda ()
			   (local-set-key (kbd "C-c C-x t") 'slack-rtm-sync-task)
			   (local-set-key (kbd "C-c C-x d") 'slack-rtm-delete-task)
			   (local-set-key (kbd "C-c C-x s") 'slack-rtm-sync)))
