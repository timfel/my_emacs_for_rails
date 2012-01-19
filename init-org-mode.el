(add-hook 'org-mode-hook (lambda () (progn
				      (local-set-key (kbd "M-RET") 'org-insert-subheading)
				      (local-set-key (kbd "C-RET") 'org-insert-heading))))
