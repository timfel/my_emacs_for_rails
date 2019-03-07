(require 'compile)
(require 'cc-mode)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("cselpa" . "https://elpa.thecybershadow.net/packages/"))
(package-initialize)

(condition-case nil
    (require 'use-package)
  (file-error
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp)))))

(setq el-get-sources
      '((:name redo+
               :type http
               :url "http://www.emacswiki.org/emacs/download/redo%2b.el"
               :load "redo_2b.el"
               :after (progn (global-set-key [(control -)] 'redo)))))

(el-get 'sync '(frame-fns
                frame-cmds
                sudo-save
                vlfi
                redo+))
(el-get 'wait)

;; packages
(use-package yasnippet
  :ensure t
  :config (yas-global-mode t))

;; additional modes I like
(use-package yaml-mode :ensure t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
            (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
            (add-to-list 'auto-mode-alist '("Gemfile.lock$" . yaml-mode))))
(use-package coffee-mode :ensure t
  :config (progn (add-hook 'coffee-mode-hook
                           '(lambda() (progn
                                        ;; Enable compile-on-save if there is already a *.coffee & *.js file
                                        (if (and (file-exists-p (buffer-file-name))
                                                 (file-exists-p (coffee-compiled-file-name)))
                                            (coffee-cos-mode t))
                                        (setq coffee-args-compile '("-c" "--bare"))
                                        (set (make-local-variable 'tab-width) 2)
                                        (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))))))
(use-package haml-mode :ensure t)
(use-package rspec-mode :ensure t)
(use-package sass-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package lua-mode :ensure t)
(use-package json-mode :ensure t)
(use-package ruby-electric :ensure t)
(use-package ruby-mode :ensure t
  :config (progn
            (add-hook 'ruby-mode-hook 'turn-on-font-lock)
            (add-hook 'ruby-mode-hook 'friendly-whitespace)
            (add-hook 'ruby-mode-hook '(lambda() (progn
                                                   (ruby-electric-mode t)
                                                   ;; Don't want flymake mode for ruby regions in rhtml files, not on read only files, or remote files
                                                   ;; (if (and (not (null buffer-file-name))
                                                   ;; 		(file-writable-p buffer-file-name)
                                                   ;; 		(not (file-remote-p buffer-file-name)))
                                                   ;; 	   (flymake-mode))
                                                   ;; Indenting options
                                                   (set (make-local-variable 'indent-tabs-mode) 'nil)
                                                   (set (make-local-variable 'tab-width) 2)
                                                   (local-set-key (kbd "<return>") 'newline-and-indent)
                                                   ;; Auto completion
                                                   (imenu-add-to-menubar "IMENU")
                                                   (setq ac-sources
                                                         '(ac-source-rsense-method
                                                           ac-source-rsense-constant
                                                           ac-source-semantic
                                                           ac-source-words-in-same-mode-buffers
                                                           ac-source-yasnippet
                                                           ac-source-abbrev))
                                                   (setq ac-omni-completion-sources
                                                         '((cons "\\.\\=" '(ac-source-rcodetools))
                                                           (cons "\\.[A-Za-z0-9_]*" '(ac-source-rsense-method))
                                                           (cons "[ :][A-Z][A-Za-z0-9_]*" '(ac-source-rsense-constant)) ))
                                                   (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
                                                   (define-key ruby-mode-map "\M-\C-o" 'rct-complete-symbol)
                                                   ;; Type inference auto completion
                                                   (if (project-current)
                                                       (rsense-open-project (project-default-directory (project-current)))))))
            (add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))))
(use-package org
  :ensure t
  :config (progn
            (add-hook 'org-mode-hook (lambda ()
                                       (progn
                                         (local-set-key (kbd "<return>") 'org-insert-heading)
                                         (local-set-key (kbd "M-<return>") 'org-insert-subheading))))
            (setq org-hide-leading-stars t)
            (setq org-agenda-files '())
            (add-to-list 'org-agenda-files (expand-file-name "~/Desktop"))
            (setq org-agenda-files (append (file-expand-wildcards
                                            (expand-file-name "~/Documents/HPI/11SS/*"))
                                           org-agenda-files))
            (setq org-insert-mode-line-in-empty-file t)))


;; tags and navigation
(use-package ggtags :ensure t)
(use-package xcscope :ensure t)
(use-package projectile :ensure t)
(use-package helm :ensure t)
(use-package helm-etags-plus
  :ensure t
  :config (progn
            (global-set-key (kbd "M-.") 'helm-etags-plus-select)))
(use-package helm-projectile
  :ensure t
  :config (progn
            (global-set-key (kbd "C-t") 'helm-projectile-find-file)))


;; Auto completion
(use-package company
  :ensure t
  :config (progn
            (global-company-mode t)
            (global-set-key (kbd "M-?") 'company-complete)))


;; Git(hub)
(use-package gist :ensure t)
(use-package magit-popup :ensure t)
(use-package magit
  :ensure t
  :config (progn
            (global-set-key (kbd "C-x C-z") 'magit-status)
            (add-hook 'magit-mode-hook 'magit-load-config-extensions)
            ;; (setq with-editor-emacsclient-executable "/usr/bin/emacsclient-snapshot")
            (setq magit-auto-revert-mode nil)))
(use-package magit-svn :ensure t)
(use-package gh :ensure t)


;; Tools
(use-package mw-thesaurus :ensure t)
(use-package cssh :ensure t)
(use-package switch-window :ensure t)
(use-package autopair :ensure t)
(use-package popup :ensure t)
(use-package fuzzy :ensure t)
(use-package pcache :ensure t)
(use-package logito :ensure t)
(use-package maxframe
  :hook (window-setup . maximize-frame))
(use-package textmate
  :ensure t
  :config (progn
            (textmate-mode 1)
            ;; Commenting blocks
            (global-set-key [(control /)] 'comment-or-uncomment-region-or-line)))
(use-package fill-column-indicator
  :ensure t
  :config (progn
            (setq fci-rule-column 81)
            ;; (setq fci-always-use-textual-rule t)
            ;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
            ;; (global-fci-mode 1)
            ))
(use-package term-keys
  :ensure t
  :unless window-system
  :config
  (progn
    (term-keys-mode t)
    (require 'term-keys-xterm)
    ;; only initially
    ;; (with-temp-buffer
    ;;  (insert (term-keys/xterm-xresources))
    ;;   (append-to-file (point-min) (point-max) "~/.Xresources"))
    ))


;; LaTeX
(use-package tex
  :ensure auctex
  :if (executable-find "pdflatex")
  :config (progn
            (defun nodbus-TeX-evince-dbus-p (de app &rest options)
              nil)
            (advice-add 'TeX-evince-dbus-p :override #'nodbus-TeX-evince-dbus-p)
            (defun dbus-register-signal (&rest args)
              nil)
            (setq-default TeX-master nil)
            (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
            (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
            (add-hook 'LaTeX-mode-hook 'reftex-mode)
            ;; (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
            (add-hook 'LaTeX-mode-hook (lambda () (auto-fill-mode -1)))
            (add-hook 'LaTeX-mode-hook 'flyspell-mode)
            (add-hook 'LaTeX-mode-hook (lambda () (setq longlines-wrap-follows-window-size t)))
            ;; (add-hook 'LaTeX-mode-hook 'longlines-mode)
            (add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\M-i" 'ispell-word)))
            (add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\M-t" 'thesaurus-choose-synonym-and-replace)))
            ;; (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
            (add-hook 'LaTeX-mode-hook 'flyspell-mode)
            (add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\M-i" 'ispell-word)))
            (setq reftex-plug-into-AUCTeX t)
            (condition-case nil
                (when
                    (add-to-list 'reftex-bibliography-commands "addbibresource"))
              (error nil))
            (setq reftex-plug-into-AUCTeX t)
            (setq TeX-auto-save t)
            (setq TeX-save-query nil)
            (setq TeX-parse-self t)))
(use-package reftex
  :ensure t
  :if (executable-find "pdflatex")
  :config (progn
            (defun bibtex ()
              (interactive)
              (find-file "~/Dropbox/Papers/bibtex.org")
              (rename-buffer "*bibtex*")
              (reftex-mode t)
              (reftex-mode nil)
              (reftex-parse-all)
              (global-auto-revert-mode t)
              ;; add a custom reftex cite format to insert links
              (reftex-set-cite-format
               '((?b . "[[bib:%l][%l-bib]]")
                 (?n . "[[notes:%l][%l]]")
                 (?p . "[[papers:%l][%l.pdf]]")
                 (?t . "%t")
                 (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l.pdf]]")))
              (local-set-key (kbd "C-x C-s") (lambda () (interactive) (write-file (buffer-file-name)) (rename-buffer "*bibtex*")))
              (local-set-key (kbd "C-c )") 'reftex-citation)
              (local-set-key (kbd "C-c (") (lambda () (org-open-link-from-string (format "[[notes:%s]]" (reftex-citation t))))))

            (setq org-link-abbrev-alist
                  '(("bib" . "~/Dropbox/Papers/bibtex.bib::%s")
                    ("notes" . "~/Dropbox/Papers/bibtex.org::#%s")
                    ("papers" . "~/Dropbox/Papers/%s.pdf")))

            (setq reftex-default-bibliography (list (expand-file-name "~/Dropbox/Papers/library.bib")))))


;; LSP and especially Java
(use-package treemacs :ensure t)
(use-package lsp-mode
  :ensure t)
  ;; :config (progn
  ;;           (add-hook 'lsp-workspace-folders-changed-hook                      
  ;;                     (lambda (added-folders removed-folders)
  ;;                       "If the LSP workspace changes, load the folders into the lsp project."
  ;;                       (let (
  ;;                             (project-name "lsp-project")
  ;;                             (paths (lsp-session-folders (lsp-session)))
  ;;                             (project (project-find "lsp-project"))
  ;;                             )
  ;;                         (if project
  ;;                             (progn
  ;;                               (project-search-paths-set project paths)
  ;;                               (project-select project-name)
  ;;                               (project-refresh))
  ;;                           (progn
  ;;                             (project-new project-name (car paths))
  ;;                             (project-search-paths-set (project-find project-name) paths)
  ;;                             (project-select project-name)
  ;;                             (project-refresh))))))))
(use-package hydra :ensure t)
(use-package company-lsp
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends #'company-lsp))
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config (progn
            (define-key lsp-mode-map (kbd "C-.") 'helm-imenu)
            (define-key lsp-mode-map (kbd "C-S-t") 'lsp-ui-find-workspace-symbol)
            (define-key lsp-mode-map (kbd "M-,") 'lsp-ui-flycheck-list)
            (define-key lsp-mode-map (kbd "M-.") 'lsp-find-definition)
            (define-key lsp-mode-map (kbd "C-M-.") 'lsp-find-references)
            (setq lsp-ui-sideline-enable t
                  lsp-ui-sideline-show-symbol t
                  lsp-ui-sideline-show-hover t
                  lsp-ui-sideline-showcode-actions t
                  lsp-ui-sideline-update-mode 'point)))
(use-package lsp-java
  :ensure t
  :after lsp
  :defer 3
  :config (progn
            (require 'lsp-ui-flycheck)
            (require 'lsp-ui-sideline)
            (setq lsp-java-workspace-dir "/home/tim/eclipse-workspace/")
            (add-hook 'java-mode-hook #'lsp)
            (add-hook 'java-mode-hook (lambda () (flycheck-mode t)))
            (add-hook 'java-mode-hook (lambda () (company-mode t)))
            (add-hook 'java-mode-hook (lambda () (lsp-ui-flycheck-enable t)))
            (add-hook 'java-mode-hook (lambda () (lsp-ui-sideline-mode t)))))



;; Custom Debug minor mode
(define-minor-mode my/dap-mode
  "My own minor mode when using the dap debugger."
  ;; The initial value - Set to 1 to enable by default
  nil
  ;; The indicator for the mode line.
  " My/DBG"
  ;; The minor mode keymap
  `(
    (,(kbd "C-c C-s") . dap-step-in)
    (,(kbd "C-c C-f") . dap-step-out)
    (,(kbd "C-c C-n") . dap-next)
    (,(kbd "C-c C-r") . dap-continue)
    (,(kbd "C-c C-d") . dap-disconnect)
    (,(kbd "C-c C-e") . dap-ui-repl)
   )
   ;; Make mode global rather than buffer local
   :global 1
)

(use-package dap-mode
  :ensure t :after lsp-mode
  :config (progn
            (dap-mode t)
            (dap-ui-mode t)

            (defun my/window-visible (b-name)
              "Return whether B-NAME is visible."
              (-> (-compose 'buffer-name 'window-buffer)
                  (-map (window-list))
                  (-contains? b-name)))

            (defun my/show-debug-windows (session)
              "Show debug windows."
              (my/dap-mode 1)
              (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
                (save-excursion
                  ;; display locals
                  (unless (my/window-visible dap-ui--locals-buffer)
                    (dap-ui-locals))
                  ;; display sessions
                  (unless (my/window-visible dap-ui--sessions-buffer)
                    (dap-ui-sessions)))))

            (add-hook 'dap-stopped-hook 'my/show-debug-windows)

            (defun my/hide-debug-windows (&optional session)
              "Hide debug windows when all debug sessions are dead."
              (my/dap-mode 0)
              (kill-buffer dap-ui--sessions-buffer)
              (kill-buffer dap-ui--locals-buffer))

            (add-hook 'dap-terminated-hook 'my/hide-debug-windows)))
(use-package dap-java
  :after (dap-mode lsp-java)
  :config (progn
            (setq dap-java-default-debug-port 8000)
            (define-key java-mode-map (kbd "C-c C-d") 'dap-debug)
            (define-key java-mode-map (kbd "C-c C-x t") 'dap-breakpoint-toggle)))


(defun my/dap-java-debug
  "Start debug session with DEBUG-ARGS."
  (interactive (list (dap-java--populate-default-args nil)))
  (dap-start-debugging debug-args))

  
(use-package lsp-java-treemacs
  :after treemacs
  :config
  (define-key lsp-mode-map (kbd "C-x t t") (lambda () (unless (eq 'visible (treemacs-current-visibility))
                                                        (lsp-java-treemacs-register)
                                                        (treemacs-select-window)))))
(use-package flycheck
  :ensure t
  :init
  (progn
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))

    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)))


;; The spacemacs default colors
(condition-case nil
    (load-theme 'spacemacs-light t)
  (error
   (package-install 'spacemacs-theme)
   (load-theme 'spacemacs-light t)))
