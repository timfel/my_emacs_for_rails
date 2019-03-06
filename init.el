

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun load-init-files (list)
  (mapcar (lambda (e) (load (expand-file-name (format "~/.emacs.d/init-%s.el" e))))
          list))

;; Run el-get
(load-init-files '(el-get-packages))
;; Some global functions, settings and modes
(load-init-files '(useful-functions windows global-settings))
;; Now setup packages that weren't loaded and/or setup through el-get
(load-init-files '(irc flyspell flymake term-mode ido server tramp bibtex skeletons darkroom-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list (quote (("Okular" "okular --unique %o#src:%n%b"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "xdg-open")
     (output-dvi "xdg-open")
     (output-pdf "Okular")
     (output-html "xdg-open"))))
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-solarized-light)))
 '(custom-safe-themes
   (quote
    ("274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(darkroom-mode-enable-fullscreen nil)
 '(doc-view-continuous t)
 '(ecb-compilation-buffer-names
   (quote
    (("*Calculator*")
     ("*vc*")
     ("*vc-diff*")
     ("*Apropos*")
     ("*Occur*")
     ("*shell*")
     ("\\*[cC]ompilation.*\\*" . t)
     ("\\*i?grep.*\\*" . t)
     ("*JDEE Compile Server*")
     ("*Help*")
     ("*Completions*")
     ("*Backtrace*")
     ("*Compile-log*")
     ("*bsh*")
     ("*Messages*")
     ("*Buffer List*"))))
 '(ecb-compile-window-height 8)
 '(ecb-compile-window-temporally-enlarge (quote both))
 '(ecb-compile-window-width (quote edit-window))
 '(ecb-layout-name "left8")
 '(ecb-layout-window-sizes
   (quote
    (("left8"
      (ecb-directories-buffer-name 0.23671497584541062 . 0.29310344827586204)
      (ecb-sources-buffer-name 0.23671497584541062 . 0.22413793103448276)
      (ecb-methods-buffer-name 0.23671497584541062 . 0.25862068965517243)
      (ecb-history-buffer-name 0.23671497584541062 . 0.20689655172413793)))))
 '(ecb-options-version "2.40")
 '(ecb-source-path
   (quote
    (("~/Devel/projects" "Dev")
     ("~/Documents/HPI/11SS/" "Uni")
     ("~/Finnlabs/Finnlabs/" "Finn"))))
 '(ecb-tree-indent 2)
 '(ecb-windows-width 0.2)
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
 '(meghanada-full-text-search-enable t)
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s"))))
 '(package-selected-packages
   (quote
    (dracula-theme magit-popup thesaurus flycheck company-lsp treemacs fill-column-indicator textmate logito pcache fuzzy popup autopair switch-window cssh gh magithub magit-svn magit gist helm-etags-plus projectile xcscope org-mode ruby-electric lua-mode markdown-mode sass-mode rspec-mode haml-mode yaml-mode org-link-minor-mode helm-projectile helm maxframe markdown-preview-mode markdown-mode+ spacemacs-theme term-keys lsp-java dap-mode lsp-ui lsp-mode tree-mode oauth2)))
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
 )
(put 'dired-find-alternate-file 'disabled nil)

(add-to-list 'term-file-aliases
             '("st-256color" . "xterm-256color")
             '("screen-256color" . "xterm-256color"))
