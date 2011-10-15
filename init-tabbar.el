;; Tabbar, this is in emacs-goodies, so it'll only work in Ubuntu
(condition-case nil
    (save-excursion
      (require 'tabbar)
      (tabbar-mode t)
      (dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
        (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))

      (defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
        `(defun ,name (arg)
           (interactive "P")
           ,do-always
           (if (equal nil arg)
               ,on-no-prefix
             ,on-prefix)))

      (defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
      (defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
      (global-set-key [(backtab)] 'shk-tabbar-next)
      (global-set-key "\C-c<left>" 'shk-tabbar-prev)
      ;; add a buffer modification state indicator in the tab label,
      ;; and place a space around the label to make it looks less crowd
      (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
        (setq ad-return-value
              (if (and (buffer-modified-p (tabbar-tab-value tab))
                       (buffer-file-name (tabbar-tab-value tab)))
                  (concat " + " (concat ad-return-value " "))
                (concat " " (concat ad-return-value " ")))))

      ;; called each time the modification state of the buffer changed
      (defun ztl-modification-state-change ()
        (tabbar-set-template tabbar-current-tabset nil)
        (tabbar-display-update))
      ;; first-change-hook is called BEFORE the change is made
      (defun ztl-on-buffer-modification ()
        (set-buffer-modified-p t)
        (ztl-modification-state-change))
      (add-hook 'after-save-hook 'ztl-modification-state-change)
      ;; this doesn't work for revert, I don't know
      ;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
      (add-hook 'first-change-hook 'ztl-on-buffer-modification)
      (custom-set-faces
       ;; custom-set-faces was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       ))
  (error nil))
