(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))                                                                   
(setq mail-header-separator "")                                                                                               
(add-hook 'message-mode-hook
          'turn-on-auto-fill
          (function
           (lambda ()
             (progn
               (local-unset-key "\C-c\C-c")
               (define-key message-mode-map "\C-c\C-c" '(lambda ()
                                                          "save and exit quickly"
                                                          (interactive)
                                                          (save-buffer)
                                                          (server-edit)))))))
(defun mutt ()
  (interactive)
  (ansi-term "mutt" "mutt"))
