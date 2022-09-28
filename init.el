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
   '("db152b961f7e6075f226a24bba7faf5b1ff016a0e614afe4e544df5ae2637b3c" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(dap-stack-trace-limit 40)
 '(darkroom-mode-enable-fullscreen nil)
 '(doc-view-continuous t)
 '(frame-background-mode 'light)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
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
     ("???" . "#dc752f")))
 '(kickasm-c64debugger-command
   "/home/tim/.emacs.d/c64debugger/c64debugger -autojmp -wait 4000")
 '(kickasm-vice-command "x64")
 '(lsp-graalvm-custom-server t)
 '(lsp-graalvm-languages '("python"))
 '(lsp-java-java-path "/home/tim/.mx/jdks/labsjdk-ee-17-jvmci-22.0-b02/bin/java")
 '(lsp-netbeans-jdk "/home/tim/.mx/jdks/labsjdk-ee-17-jvmci-22.0-b02")
 '(lsp-tcp-connection-timeout 20)
 '(lsp-ui-doc-position 'top)
 '(markdown-command "cmark-gfm --extension table")
 '(org-agenda-files
   '("/home/tim/OneDrive/todo.org" "/home/tim/OneDrive/notes.org"))
 '(org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s")))
 '(package-selected-packages
   '(lsp-java lsp-treemacs treemacs lsp-pyright termbright-theme quelpa quelpa-use-package visual-fill-column forge sx narrow-indirect which-key jsonnet-mode dumb-jump erefactor auth-source-xoauth2 csharp-mode org-caldav calfw calfw-org wl wanderlust posframe lsp-java-treemacs treemacs-icons company-box doom-modeline dracula-theme magit-popup thesaurus flycheck company-lsp fill-column-indicator textmate logito pcache fuzzy popup autopair switch-window cssh gh magithub magit-svn magit gist helm-etags-plus projectile xcscope org-mode ruby-electric lua-mode markdown-mode sass-mode rspec-mode haml-mode yaml-mode org-link-minor-mode helm-projectile helm markdown-preview-mode markdown-mode+ spacemacs-theme term-keys tree-mode oauth2))
 '(pdf-view-midnight-colors '("#655370" . "#fbf8ef"))
 '(proced-auto-update-interval 2)
 '(safe-local-variable-values
   '((flycheck-disabled-checkers emacs-lisp-checkdoc)
     (reftex-default-bibliography "fallback.bib")
     (whitespace-line-column . 80)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
                     (lambda nil
                       (delete-trailing-whitespace)
                       nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-style face tabs trailing lines-tail)
     (mode . org)
     (encoding . us-ascii)
     (encoding . utf-8)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((comp))))
;; resize the windows on emacs and run ecb-store-window-sizes
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-sideline-code-action ((t (:foreground "firebrick")))))
