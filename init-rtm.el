;; (defun gtd ()
;;   (interactive)
;;   (el-get-init "slack-rtm")
;;   (if (eq nil (get-buffer slack-rtm-buffer-name))
;;       (slack-rtm slack-rtm-default-query))
;;   (switch-to-buffer-other-window slack-rtm-buffer-name))

;; (defun slack-rtm-reload-buffer ()
;;   (let ((latest-buffer-name (generate-new-buffer-name slack-rtm-buffer-name)))
;;     (save-selected-window
;;       (slack-rtm slack-rtm-default-query)
;;       (buffer-swap-text (get-buffer latest-buffer-name))
;;       (kill-buffer latest-buffer-name))))

;; (global-set-key "\C-x\C-g" 'gtd)

;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (if (string= (buffer-name) "*todo*")
;; 		(progn
;; 		  (local-set-key (kbd "<S-right>") (lambda () (interactive) (slack-rtm-touch-task) (org-shiftright)))
;; 		  (local-set-key (kbd "<S-left>") (lambda () (interactive) (slack-rtm-touch-task) (org-shiftleft)))
;; 		  (local-set-key (kbd "C-x C-s") (lambda () (interactive) (slack-rtm-sync) (slack-rtm-reload-buffer)))
;; 		  (local-set-key (kbd "C-x C-v") 'slack-rtm-reload-buffer)
;; 		  (local-set-key (kbd "C-x C-d") 'slack-rtm-delete-task)
;; 		  (local-set-key (kbd "C-x C-n") 'slack-rtm-task-quickadd)
;; 		  (local-set-key (kbd "C-t") 'slack-rtm-touch-task)))))

(global-set-key "\C-x\C-g" 'simple-rtm-mode)
