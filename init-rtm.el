(defun gtd ()
  (interactive)
  (el-get-init "slack-rtm")
  (if (eq nil (get-buffer "*todo*"))
      (slack-rtm slack-rtm-default-query))
  (switch-to-buffer-other-window "*todo*"))

(global-set-key "\C-x\C-g" 'gtd)

(add-hook 'org-mode-hook (lambda ()
			   (if (string= (buffer-name) "*todo*")
			       (progn
				 (local-set-key (kbd "<S-right>") (lambda () (interactive) (slack-rtm-touch-task) (org-shiftright)))
				 (local-set-key (kbd "<S-left>") (lambda () (interactive) (slack-rtm-touch-task) (org-shiftleft)))
				 (local-set-key (kbd "C-x C-s") 'slack-rtm-sync)
				 (local-set-key (kbd "C-x C-d") 'slack-rtm-delete-task)
				 (local-set-key (kbd "C-t") 'slack-rtm-touch-task)))))
