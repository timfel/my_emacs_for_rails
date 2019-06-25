(package-initialize)
(load (expand-file-name (locate-user-emacs-file "init-packages.el")))
(load (expand-file-name (locate-user-emacs-file "init-my-functions.el")))
(load (expand-file-name (locate-user-emacs-file "init-my-global-settings.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(darkroom-mode-enable-fullscreen nil)
 '(doc-view-continuous t)
 '(frame-background-mode (quote light))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")
     ("???" . "#dc752f"))))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s"))))
 '(package-selected-packages
   (quote
    (lsp-java-treemacs treemacs-icons company-box doom-modeline dracula-theme magit-popup thesaurus flycheck company-lsp treemacs fill-column-indicator textmate logito pcache fuzzy popup autopair switch-window cssh gh magithub magit-svn magit gist helm-etags-plus projectile xcscope org-mode ruby-electric lua-mode markdown-mode sass-mode rspec-mode haml-mode yaml-mode org-link-minor-mode helm-projectile helm markdown-preview-mode markdown-mode+ spacemacs-theme term-keys lsp-java dap-mode lsp-ui lsp-mode tree-mode oauth2)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(safe-local-variable-values
   (quote
    ((reftex-default-bibliography "fallback.bib")
     (whitespace-line-column . 80)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-style face tabs trailing lines-tail)
     (mode . org)
     (encoding . us-ascii)
     (encoding . utf-8))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
;; resize the windows on emacs and run ecb-store-window-sizes
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-sideline-code-action ((t (:foreground "firebrick")))))
