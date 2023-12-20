
(package-initialize)
(require 'compile)
(require 'cc-mode)
(require 'hl-line)
(require 'gud)

(setq warning-minimum-level :error)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("cselpa" . "https://elpa.thecybershadow.net/packages/"))

(setq use-package-verbose t)

(condition-case nil
    (require 'use-package)
  (file-error
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(use-package ht
  :ensure t
  :demand t)

;; additional modes I like
(use-package yaml-mode :ensure t
  :mode ("\\.yml$" "\\.yaml$" "Gemfile.lock$"))
(use-package coffee-mode :ensure t
  :mode ("\\.coffee$")
  :config (progn (add-hook 'coffee-mode-hook
                           '(lambda() (progn
                                        ;; Enable compile-on-save if there is already a *.coffee & *.js file
                                        (if (and (file-exists-p (buffer-file-name))
                                                 (file-exists-p (coffee-compiled-file-name)))
                                            (coffee-cos-mode t))
                                        (setq coffee-args-compile '("-c" "--bare"))
                                        (set (make-local-variable 'tab-width) 2)
                                        (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))))))
(use-package haml-mode :ensure t
  :mode ("\\.haml$"))
(use-package rspec-mode :ensure t :after ruby)
(use-package sass-mode :ensure t :defer t)
(use-package markdown-mode :ensure t
  :config (setq markdown-command "cmark-gfm --extension table")
  :mode ("\\.md$"))
(use-package lua-mode :ensure t
  :config (progn
            (require 'lsp)
            (require 'lsp-lua)
            (if (not (f-exists-p lsp-clients-emmy-lua-jar-path))
                (progn
                  (mkdir (f-dirname lsp-clients-emmy-lua-jar-path) t)
                  (url-copy-file
                   "https://github.com/EmmyLua/EmmyLua-LanguageServer/releases/download/0.3.6/EmmyLua-LS-all.jar"
                   lsp-clients-emmy-lua-jar-path))))
  :mode ("\\.lua$"))
(use-package json-mode :ensure t
  :mode ("\\.json$"))
(use-package ruby-electric :ensure t :after ruby)
(use-package ruby-mode :ensure t
  :mode ("\\.rb$")
  :config (progn
            ;; Patch ruby-mode
            (defun ruby-accurate-end-of-block (&optional end)
              "(tfel): Fixes an issue I had with ruby-mode."
              (let (state
                    (end (or end (point-max))))
                (while (and (setq state (apply 'ruby-parse-partial end state))
                            (nth 2 state) (>= (nth 2 state) 0) (< (point) end)))))
            
            (add-hook 'ruby-mode-hook 'turn-on-font-lock)
            (add-hook 'ruby-mode-hook 'friendly-whitespace)
            (add-hook 'ruby-mode-hook '(lambda() (progn
                                                   (ruby-electric-mode t)
                                                   ;; Indenting options
                                                   (set (make-local-variable 'indent-tabs-mode) 'nil)
                                                   (set (make-local-variable 'tab-width) 2)
                                                   (local-set-key (kbd "<return>") 'newline-and-indent)
                                                   ;; Auto completion
                                                   (imenu-add-to-menubar "IMENU")
                                                   (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
                                                   (define-key ruby-mode-map "\M-\C-o" 'rct-complete-symbol))))
            (add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))))

(use-package org
  :commands org-mode
  :mode ("\\.org$")
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config (progn
            (define-key org-mode-map (kbd "C-c <right>") #'org-shiftright)
            (define-key org-mode-map (kbd "C-c <left>") #'org-shiftleft)
            (define-key org-mode-map (kbd "C-c M-RET") #'org-insert-subheading)
            (add-hook 'org-mode-hook (lambda () (run-at-time "1 sec" nil (lambda () (fci-mode 0)))))
            (require 'org-tempo)
            (let ((todos (if (eq system-type 'windows-nt)
                             (expand-file-name "~/../../OneDrive/todo.org")
                           (expand-file-name "~/OneDrive/todo.org")))
                  (notes (if (eq system-type 'windows-nt)
                             (expand-file-name "~/../../OneDrive/notes.org")
                           (expand-file-name "~/OneDrive/notes.org"))))
              (setq
               org-file-apps '((auto-mode . emacs)
                              ("\\.mm\\'" . default)
                              ("\\.x?html?\\'" . default)
                              ("\\.pdf\\'" . "evince %s"))
               org-replace-disputed-keys t
               org-default-notes-file notes
               org-agenda-files (list todos notes)
               ;; warn me of any deadlines in next 7 days
               org-deadline-warning-days 7
               ;; show me tasks scheduled or due in next fortnight
               org-agenda-span 'fortnight
               ;; don't show tasks as scheduled if they are already shown as a deadline
               org-agenda-skip-scheduled-if-deadline-is-shown t
               ;; don't give awarning colour to tasks with impending deadlines if they are
               ;; scheduled to be done
               org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
               ;; don't show tasks that are scheduled or have deadlines in the normal todo
               ;; list
               org-agenda-todo-ignore-deadlines 'all
               org-agenda-todo-ignore-scheduled 'all
               ;; sort tasks in order of when they are due and then by priority
               org-agenda-sorting-strategy '((agenda deadline-up priority-down)
                                             (todo priority-down category-keep)
                                             (tags priority-down category-keep)
                                             (search category-keep))
               org-insert-mode-line-in-empty-file t
               ;; do not shift lower items
               org-adapt-indentation nil
               ;; i like this more
               org-hide-leading-stars t
               org-highest-priority ?A
               org-lowest-priority ?C
               org-default-priority ?A
               org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                                    (?B . (:foreground "LightSteelBlue"))
                                    (?C . (:foreground "OliveDrab")))
               org-agenda-window-setup 'current-window
               org-clock-idle-time 15
               org-capture-templates
               (list
                ;; schedule new todo items to today by default
                (list "n" "note" 'entry (list 'file+datetree notes) "* %?\nEntered on %U\n")
                (list "t" "todo" 'entry (list 'file+headline todos "Tasks") "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"))))))

(use-package org-download
  :ensure t
  :after org
  :commands org-screenshot
  :config (progn
            (defun org-screenshot ()
              (interactive)
              (call-interactively #'org-download-screenshot))
            (setq
             org-image-actual-width (list 600)
             org-download-image-org-width 200
             org-download-screenshot-method (expand-file-name "~/bin/wslscr.py %s"))
            (set-default 'org-download-image-dir (expand-file-name "~/OneDrive/Screenshots/"))))

;; tags and navigation
;; (use-package ggtags :ensure t)
;; (use-package xcscope :ensure t)
(use-package projectile
  :ensure t
  :config (progn
            (setq
             projectile-indexing-method 'alien
             projectile-sort-order 'access-time
             projectile-enable-caching nil)
            (add-to-list 'projectile-globally-ignored-directories "^mxbuild$")
            (add-to-list 'projectile-globally-ignored-directories "^eln-cache$")
            (add-to-list 'projectile-globally-ignored-directories "^eevenv$")
            (add-to-list 'projectile-globally-ignored-directories "*site-packages")))

(use-package helm
  :ensure t
  :demand t)

(use-package helm-etags-plus
  :disabled
  :ensure t
  :bind (("M-." . helm-etags-plus-select)))

(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :bind (("C-t" . helm-projectile-find-file)))

;; Auto completion
(use-package yasnippet
  :ensure t
  :hook ((lsp-mode . yas-minor-mode)
         (python-mode . yas-minor-mode)))

(use-package company
  :ensure t
  :demand t
  :bind (("M-?" . company-complete))
  :config (progn
            (global-company-mode t)
            (setq
             company-dabbrev-downcase 0
             company-idle-delay (if (eq window-system 'w32) 10 0.2))))

(use-package company-box
  :ensure t
  :after company
  :if (window-system)
  :hook (company-mode . company-box-mode))

(use-package magit
  :bind ("C-x C-z" . magit-status)
  :ensure t
  :config (progn
            ;; (add-hook 'magit-mode-hook 'magit-load-config-extensions)
            ;; (setq with-editor-emacsclient-executable "/usr/bin/emacsclient-snapshot")
            (setq magit-auto-revert-tracked-only t)
            (magit-auto-revert-mode)
            (if (eq system-type 'windows-nt)
                ;; These also help on older git, they are default these days on Windows
                ;; git config --global core.preloadindex true
                ;; git config --global core.fscache true
                ;; git config --global gc.auto 256
                (progn
                  ;; reduce the number of things in the status buffer to reduce calls
                  (setq magit-refresh-status-buffer nil)
                  (mapcar (lambda (x) (delete x magit-status-sections-hook))
                          (list 'magit-insert-am-sequence
                                'magit-insert-sequencer-sequence
                                'magit-insert-bisect-output
                                'magit-insert-bisect-rest 
                                'magit-insert-bisect-log
                                'magit-insert-stashes
                                'magit-insert-unpushed-to-pushremote
                                'magit-insert-unpushed-to-upstream-or-recent
                                'magit-insert-unpulled-from-pushremote
                                'magit-insert-unpulled-from-upstream))
                  ))))

;; Tools
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

(use-package fic-mode
  :ensure t
  :demand t)

(use-package request
  :ensure t
  :defer t)

(use-package mw-thesaurus
  :ensure t
  :commands (mw-thesaurus-lookup mw-thesaurus-lookup-at-point mw-thesaurus-lookup-dwim)
  :after request)

;; (use-package switch-window :ensure t)

(use-package popup :ensure t)

(use-package fuzzy :ensure t)

(use-package pcache :ensure t)

(use-package logito :ensure t)

(use-package textmate
  :ensure t
  :bind (("C-/" . comment-or-uncomment-region-or-line))
  :demand t
  :config (progn
            (textmate-mode 1)))

(use-package all-the-icons
  :demand t
  :ensure t)

(use-package all-the-icons-completion
  :after all-the-icons
  :demand t
  :ensure t)

(use-package all-the-icons-dired
  :after all-the-icons
  :demand t
  :ensure t)

(use-package doom-modeline
  ;; remember to run (all-the-icons-install-fonts) manually some time
  :demand t
  :after all-the-icons
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config (setq doom-modeline-minor-modes nil
                doom-modeline-minor-modes t
                doom-modeline-bar-width 4
                doom-modeline-hud nil
                doom-modeline-vcs-max-length 28
                doom-modeline-lsp t
                doom-modeline-buffer-file-name-style 'truncate-all))

;; LaTeX
(use-package auctex
  :mode ("\\.tex$")
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
  :after tex
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
(use-package treemacs
  ;; :pin melpa-stable
  :ensure t
  :config
  (progn
    (setq treemacs-file-follow-delay 1.0
          treemacs-width 45
          treemacs-width-is-initially-locked t)))

(use-package which-key
  :ensure t
  :config (progn
            (which-key-mode)
            (which-key-setup-side-window-right-bottom)))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :preface (setq lsp-use-plists (not (eq window-system 'w32)))
  :ensure t
  :demand t
  :config (progn
            (setq lsp-print-io nil
                  lsp-lens-enable (not (eq system-type 'windows-nt))
                  lsp-completion-enable-additional-text-edit t
                  lsp-enable-snippet t
                  lsp-enable-indentation nil
                  lsp-before-save-edits t
                  lsp-enable-file-watchers nil)))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :config (progn
            (define-key lsp-mode-map (kbd "C-.") #'helm-imenu)
            ;; (define-key lsp-mode-map (kbd "C-S-t") #'lsp-ui-find-workspace-symbol)
            (define-key lsp-mode-map (kbd "C-S-t") #'helm-lsp-workspace-symbol)
            (define-key lsp-mode-map (kbd "C-c f") (lambda () (interactive) (list-flycheck-errors)))
            (define-key lsp-ui-mode-map (kbd "C-c e") #'lsp-treemacs-errors-list)
            (define-key lsp-mode-map (kbd "C-,") #'lsp-execute-code-action)
            ;; (lsp-ui-peek-jump-backward)
            ;; (lsp-ui-peek-jump-forward)
            (define-key lsp-ui-mode-map (kbd "M-,") #'xref-pop-marker-stack)
            (define-key lsp-ui-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
            (define-key lsp-ui-mode-map (kbd "C-M-.") #'lsp-ui-peek-find-references)
            ;; (define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
            ;; (define-key lsp-mode-map (kbd "C-M-.") #'lsp-find-references)
            ;; performance tips from readme
	    (defun my-minibuffer-setup-hook ()
	       (setq gc-cons-threshold most-positive-fixnum))
            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold 10000000))
            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
            (setq lsp-prefer-capf t)
            (setq read-process-output-max (* 1024 1024)) ;; 1mb
            ;; settings
            (setq lsp-ui-flycheck-live-reporting t
                  lsp-print-performance nil
                  lsp-enable-symbol-highlighting (not (eq system-type 'windows-nt)) ;; XXX: crashes me often!
                  lsp-enable-links (not (eq system-type 'windows-nt)) ;; XXX: crashes me often!
                  lsp-report-if-no-buffer t
                  lsp-enable-snippet t
                  lsp-enable-xref t
                  lsp-completion-enable t
                  lsp-completion-filter-on-incomplete nil
                  lsp-completion-show-detail (not (eq system-type 'windows-nt))
                  lsp-completion-show-kind nil
                  lsp-completion-sort-initial-results nil
                  lsp-response-timeout 30
                  lsp-diagnostic-clean-after-change nil
                  lsp-eldoc-render-all t
                  lsp-ui-peek-always-show (not (eq window-system 'w32))
                  lsp-ui-doc-enable (not (eq window-system 'w32))
                  lsp-ui-doc-max-height 30
                  lsp-ui-doc-position 'top
                  lsp-ui-doc-use-webkit (not (eq window-system 'w32))
                  lsp-ui-doc-show-with-cursor nil
                  lsp-ui-sideline-enable (not (eq system-type 'windows-nt))
                  lsp-ui-sideline-show-symbol nil
                  lsp-ui-sideline-show-hover (not (eq window-system 'w32))
                  lsp-ui-sideline-show-code-actions t
                  lsp-ui-sideline-ignore-duplicate t
                  lsp-ui-sideline-delay 2
                  lsp-eldoc-enable-hover nil
                  lsp-idle-delay 5.000
                  lsp-tcp-connection-timeout 20
                  lsp-modeline-diagnostics-enable nil
                  lsp-modeline-code-actions-enable nil
                  lsp-ui-sideline-code-actions-prefix ""
                  lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default
                  lsp-ui-sideline-update-mode 'line)))

(use-package lsp-treemacs
  :ensure t
  :demand t
  :commands lsp-treemacs-errors-list)

(use-package lsp-pyright
  :ensure t
  :after (lsp-mode dap-mode)
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (require 'dap-python)))
  :config (progn
            (setq
             dap-python-debugger 'debugpy)

            (defun get-venv-executable (orig-fun command)
              (let* ((root (lsp-workspace-root (buffer-file-name)))
                     (cfg (f-join root "pyrightconfig.json")))
                (if (f-exists? cfg) ; have a pyrightconfig.json, parse it
                    (let* ((json (with-temp-buffer (insert-file-contents cfg) (json-parse-buffer)))
                           (venvPathCfg (ht-get json "venvPath" lsp-pyright-venv-directory))
                           (venvCfg (ht-get json "venv" ""))
                           (exe (if (f-absolute? venvPathCfg)
                                    (f-join venvPathCfg venvCfg "bin" command)
                                  (f-join root venvPathCfg venvCfg "bin" command))))
                      (if (f-executable? exe) ; venv from pyrightconfig.json has the desired executable
                          exe
                        (if lsp-pyright-venv-path ; venv is set via pyright setting
                            (let ((exe (f-join lsp-pyright-venv-path "bin" command)))
                              (if (f-executable? exe) ; venv from pyright settings has the desired executable
                                  exe
                                (funcall orig-fun command)))))))))
            (advice-add 'dap-python--pyenv-executable-find :around #'get-venv-executable)))

;; Global Java settings
(add-hook 'java-mode-hook
          (lambda ()
            (set-fill-column 99)
            (c-set-offset 'substatement-open 0)
            (c-set-offset 'case-label '+)
            (c-set-offset 'arglist-close 0)
            (if (assoc 'inexpr-class c-offsets-alist)
                (c-set-offset 'inexpr-class 0))
            (c-set-offset 'arglist-cont-nonempty
                          (lambda (syntax)
                            (save-excursion
                              (if (and (= (length c-syntactic-context) 2)
                                       (eq (caar c-syntactic-context) 'arglist-cont-nonempty)
                                       (or
                                        (eq (caadr c-syntactic-context) 'statement-block-intro)
                                        (eq (caadr c-syntactic-context) 'block-close)))
                                  0
                                16))))))
(define-key java-mode-map (kbd "C-S-o") #'lsp-java-organize-imports)
(add-hook 'java-mode-hook 'friendly-whitespace)
;; (add-hook 'java-mode-hook (lambda () (flycheck-mode t)))
(add-hook 'java-mode-hook (lambda () (company-mode t)))

;; bind C-c C-d dynamically
(fset 'my/dap-debug 'dap-debug)

(defun my/toggle-dap-hydra ()
  (interactive)
  (if hydra-curr-map
      (dap-hydra/nil)
    (dap-hydra/body)))

(add-hook 'dap-session-created-hook
          (lambda (arg) (fset 'my/dap-debug 'my/toggle-dap-hydra)))
(add-hook 'dap-terminated-hook
          (lambda (arg)
            (dap-hydra/nil)
            (fset 'my/dap-debug 'dap-debug)))

(defun treemacs-t ()
  (interactive)
  (treemacs t))

(use-package iedit
  :demand t
  :ensure t)

(use-package lsp-java
  :ensure t
  :hook (java-mode . (lambda () (require 'lsp-java) (require 'dap-java)))
  :bind (("<f5>" . treemacs-t))
  :after (lsp-mode company lsp-treemacs)
  :config (progn
            (require 'lsp-ui-flycheck)
            (require 'lsp-ui-sideline)
            (setq lsp-java-completion-favorite-static-members (vconcat lsp-java-completion-favorite-static-members
                                                                       '("com.oracle.graal.python.builtins.PythonBuiltinClassType"
                                                                         "com.oracle.graal.python.nodes.BuiltinNames"
                                                                         "com.oracle.graal.python.nodes.SpecialMethodNames"
                                                                         "com.oracle.graal.python.nodes.SpecialAttributeNames"
                                                                         "com.oracle.graal.python.nodes.ErrorMessages")))
            (if (not (eq window-system 'w32))
                (setq
                 lsp-java-java-path "/home/tim/.mx/jdks/labsjdk-ce-21-jvmci-23.1-b22/bin/java"))
            (setq
             lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true")
             lsp-java-content-provider-preferred "fernflower"
             lsp-java-save-actions-organize-imports t
             lsp-java-format-on-type-enabled nil
             lsp-java-format-comments-enabled nil
             lsp-java-format-enabled nil
             lsp-java-autobuild-enabled nil
             lsp-java-inhibit-message t
             lsp-java-completion-import-order ["java" "javax" "org" "com"]
             lsp-java-import-order ["java" "javax" "org" "com"])

	    (setq dap-java-default-debug-port 8000)
            (with-eval-after-load 'lsp-treemacs
              (dap-register-debug-template "Java Attach com.oracle.graal.python"
                                           (list :type "java"
                                                 :request "attach"
                                                 :hostName "localhost"
                                                 :projectName "com.oracle.graal.python"
                                                 :port 8000)))

            (defun lsp-java--treemacs-sync ()
              (let* ((wsname (treemacs-workspace->name (treemacs-current-workspace)))
                     (wsuserdir (f-join lsp-server-install-dir
                                        (format "jdtls.workspace.%s" wsname))))
                (if (not (equal lsp-java-workspace-dir wsuserdir))
                    (progn
                      ;; shutdown servers
                      (->> (lsp-session)
                           (lsp-session-folder->servers)
                           (hash-table-values)
                           (-flatten)
                           (-uniq)
                           (-map #'lsp-workspace-shutdown))
                      (setq lsp--session nil)
                      (setq
                       lsp-java-workspace-dir wsuserdir
                       lsp-session-file (expand-file-name (locate-user-emacs-file (format ".lsp-session-v1-%s" wsname)))))))
              (message (format "Setting Eclipse workspace to %s, session to %s" lsp-java-workspace-dir lsp-session-file))
              (message (format "You may have to adapt %s/.metadata/.plugins/org.eclipse.core.runtime/.settings/org.eclipse.jdt.launching.prefs to give the default VM the name that mx told you" lsp-java-workspace-dir))
              lsp-java-workspace-dir)

            (with-eval-after-load 'lsp-treemacs
              (add-hook 'treemacs-switch-workspace-hook #'lsp-java--treemacs-sync)
              (lsp-java--treemacs-sync))

            (defun lsp-if-jdtls-running ()
              (if (lsp-find-workspace 'jdtls nil) (lsp)))
            (add-hook 'java-mode-hook #'lsp-if-jdtls-running)))

(defun my/lsp/find-eclipse-projects-recursively (directory)
  (let ((current-directory-list (directory-files directory)))
    (seq-concatenate 'list
     (if (seq-some (lambda (elt) (string-equal ".project" elt)) current-directory-list)
         (list directory)
       '())
      (seq-mapcat (lambda (elt) (my/lsp/find-eclipse-projects-recursively (concat (file-name-as-directory directory) elt)))
                  (seq-filter (lambda (elt) (and (file-directory-p (concat (file-name-as-directory directory) elt))
                                                 (not (f-symlink-p (concat (file-name-as-directory directory) elt)))
                                                 (not (string-prefix-p "." elt))
                                                 (not (string-prefix-p "mxbuild" elt))
                                                 (not (string-prefix-p "mx." elt)))) current-directory-list)))))

(defun my/lsp/reload-all-java-buffers ()
  (interactive)
  (let ((list (buffer-list)))
    (dolist (buffer list)
      (let ((name (buffer-name buffer))
            (mode (with-current-buffer buffer major-mode)))
        (if (eq mode 'java-mode)
            (if (not (buffer-modified-p buffer))
                (with-current-buffer buffer
                  (if (funcall buffer-stale-function)
                      (progn
                        (message "Reverting %s" (buffer-name))
                        (revert-buffer :ignore-auto :noconfirm))))))))))

(defun my/lsp/kill-old-java-buffers ()
  (interactive)
  (let ((list (buffer-list))
        (recent-cnt 0))
    (dolist (buffer list)
      (let ((name (buffer-name buffer))
            (mode (with-current-buffer buffer major-mode)))
        (if (eq mode 'java-mode)
            (progn 
              (setq recent-cnt (+ 1 recent-cnt))
              (if (and (not (buffer-modified-p buffer))
                       (> recent-cnt 5))
                  (kill-buffer buffer))))))))

(defun my/lsp/kill-all-java-buffers ()
  (interactive)
  (let ((list (buffer-list)))
    (dolist (buffer list)
      (let ((name (buffer-name buffer))
            (mode (with-current-buffer buffer major-mode)))
        (if (eq mode 'java-mode)
            (if (and (not (buffer-modified-p buffer))
                     (not (eq (current-buffer) buffer)))
                (kill-buffer buffer)))))))

(defun my/lsp/clear-workspace ()
  (interactive)
  (seq-do (lambda (elt)
            (lsp-workspace-folders-remove elt))
          (lsp-session-folders (lsp-session)))
  (puthash 'jdtls
           '()
           (lsp-session-server-id->folders (lsp-session)))
  (lsp--persist-session (lsp-session)))

(defun my/lsp/import-eclipse-projects ()
  (interactive)
  (let* ((base-dir (read-directory-name "Base directory to search projects in: "))
         (base-dirs (completing-read-multiple "Base sub-directories to search projects in: " (directory-files base-dir) nil t))
         (projects-found (seq-mapcat (lambda (elt) (my/lsp/find-eclipse-projects-recursively (concat (file-name-as-directory base-dir) elt))) base-dirs))
         (projects-to-import (completing-read-multiple "Select projects to import (comma-sep, * for all): " projects-found nil nil))
         (additional-required-projects '())
         (go-again t))
    (require 'xml)
    (if (equal projects-to-import '("*"))
        (setq projects-to-import projects-found))
    (setq projects-to-import
          (mapcan (lambda (elt)
                    (if (string-suffix-p "*" elt)
                        (seq-filter (lambda (elt2) (string-prefix-p (substring elt 0 (- (length elt) 1)) elt2)) projects-found)
                      (list elt)))
                  projects-to-import))
    (while go-again
      (setq go-again (length projects-to-import))
      (seq-do (lambda (elt)
                (progn
                  ;; find the required projects for each selected project
                  (seq-do
                   (lambda (elt)
                     (add-to-list 'additional-required-projects (car (xml-node-children elt))))
                   (xml-get-children
                    (car
                     (xml-get-children 
                      (assq 'projectDescription (xml-parse-file (concat (file-name-as-directory elt) ".project")))
                      'projects))
                    'project))
                  ;; find JAR projects on the factorypath that are part of a
                  ;; workspace project no go through
                  (if (file-exists-p (concat (file-name-as-directory elt) ".factorypath"))
                      (seq-do (lambda (elt)
                                (if (string-equal (xml-get-attribute-or-nil elt 'kind) "WKSPJAR")
                                    (add-to-list 'additional-required-projects (cadr (split-string (xml-get-attribute elt 'id) "/")))))
                              (xml-get-children
                               (assq 'factorypath (xml-parse-file (concat (file-name-as-directory elt) ".factorypath")))
                               'factorypathentry)))))
              projects-to-import)

      ;; resolve dependencies
      (seq-do (lambda (elt)
                (let ((name (car (xml-node-children (car (xml-get-children 
                                                          (assq 'projectDescription (xml-parse-file (concat (file-name-as-directory elt) ".project")))
                                                          'name))))))
                  (if (seq-contains-p additional-required-projects name)
                      (add-to-list 'projects-to-import elt))))
              projects-found)
      ;; if we added projects to the list of projects to import, go deeper
      (setq go-again (> (length projects-to-import) go-again)))

    ;; add projects to session
    (dolist (elt projects-to-import)
      (let ((exp (expand-file-name elt)))
        (if (not (seq-contains-p (lsp-session-folders (lsp-session)) exp))
            (progn
              (lsp-workspace-folders-add exp)
              (puthash 'jdtls
                       (append (gethash 'jdtls
                                        (lsp-session-server-id->folders (lsp-session)))
                               (list exp))
                       (lsp-session-server-id->folders (lsp-session)))
              ))))
    (lsp--persist-session (lsp-session))
    (seq-do (lambda (elt) (message (format "Imported '%s'" elt))) projects-to-import)))

(defun my/lsp/rebuild-java ()
  (interactive)
  (my/lsp/reload-all-java-buffers)
  (lsp-send-notification
   (lsp-make-request "java/buildWorkspace" t)))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :bind (("C-c C-d" . my/dap-debug)
         :map java-mode-map
         ("C-c C-d" . my/dap-debug))
  :config (progn
            ;; ;; XXX: workaround for some weird behaviour only I am seeing  ¯\_(ツ)_/¯ 
            ;; (defun my/dap--debug-session-workspace (origfunc session)
            ;;   (or (funcall origfunc session)
            ;;       (seq-some #'identity (lsp-workspaces))
            ;;       (lsp-find-workspace 'jdtls nil)))
            ;; (advice-add 'dap--debug-session-workspace :around #'my/dap--debug-session-workspace)

            (defun my/show-debug-windows (session)
              (save-excursion
                (call-interactively #'dap-ui-repl)
                (call-interactively #'dap-ui-breakpoints)
                ;; (call-interactively #'dap-ui-locals)
                ;; (call-interactively #'dap-ui-sessions)
                (if (get-buffer-window "*dap-ui-repl*")
                    (delete-other-windows)
                    (delete-window (get-buffer-window "*dap-ui-repl*")))
                (display-buffer-in-side-window (get-buffer "*dap-ui-repl*") `((side . bottom)
                                                                              (slot . 1)
                                                                              (window-height . 10)))))
            (add-hook 'dap-session-created-hook 'my/show-debug-windows)

            (defun my/close-debug-windows (session)
              (condition-case nil
                  (if (get-buffer-window "*dap-ui-repl*")
                      (delete-window (get-buffer-window "*dap-ui-repl*")))))
            (add-hook 'dap-terminated-hook 'my/close-debug-windows)

            (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
            (add-hook 'dap-terminated-hook (lambda (arg) (call-interactively #'dap-hydra/nil)))

            ;; default settings
            (setq
             dap-stack-trace-limit 40
             dap-auto-configure-features '(sessions tooltip)
             dap-auto-show-output t
             dap-print-io nil)
            (dap-auto-configure-mode)
            ;; (require 'dap-gdb-lldb)
            ))

(use-package dap-gdb-lldb
  :after dap-mode
  :hook ((c-mode c++-mode) . (lambda () (require 'dap-gdb-lldb))))

(use-package dap-lldb
  :after dap-mode
  :hook ((c-mode c++-mode) . (lambda () (require 'dap-lldb)))
  :config (progn
            (setq
             dap-lldb-debug-program
             `(,(expand-file-name "~/.emacs.d/llvm-project/lldb/build/bin/lldb-vscode")))

            (defun dap-cppdbg-gdb-attach (file)
              (interactive "fPath to running executable: ")
              (let ((pid (shell-command-to-string (format "pidof -s %s" (f-base file)))))
                (progn
                  (message (format "%s" pid))
                  (dap-debug (list :type "cppdbg"
                                   :request "attach"
                                   :program (expand-file-name file)
                                   :MIMode "gdb"
                                   :processId pid
                                   :name "cpptools::Attach GDB")))))

            (defun dap-cppdbg-lldb-attach (file)
              (interactive "fPath to running executable: ")
              (let ((pid (shell-command-to-string (format "pidof %s" (f-base file)))))
                (progn
                  (message (format "%s" pid))
                  (dap-debug (list :type "cppdbg"
                                   :request "attach"
                                   :program (expand-file-name file)
                                   :MIMode "lldb"
                                   :processId pid
                                   :name "cpptools::Attach LLDB")))))

            (defun dap-lldb-attach (file)
              (interactive "fPath to running executable: ")
              (let ((pid (shell-command-to-string (format "pidof %s" (f-base file)))))
                (progn
                  (message (format "%s" pid))
                  (dap-debug (list :type "lldb-vscode"
                                   :request "attach"
                                   :program (expand-file-name file)
                                   :pid pid
                                   :stopOnEntry nil
                                   :name "LLDB::Attach")))))))

(use-package dap-hydra
  :defer t
  :after dap-mode)

(use-package dap-cpptools  
  :defer t
  :hook ((c-mode c++-mode) . (lambda () (require 'dap-cpptools)))
  :after dap-mode)

(use-package csharp-mode
  :mode ("\\.cs$")
  :ensure t)

(use-package dap-node
  :after dap-mode
  :config (dap-register-debug-template
           "Node Attach 9229"
           (list :type "node"
                 :cwd nil
                 :request "attach"
                 :protocol "auto"
                 :address "127.0.0.1"
                 :stopOnEntry t
                 :port 9229
                 :program ""
                 :name "Node Attach 9229")))

(use-package helm-lsp
  :ensure t
  :after (helm lsp-mode)
  :commands helm-lsp-workspace-symbol)

;; The spacemacs default colors
(let ((theme (cond ((eq window-system 'w32)
                    'eclipse)
                   ((eq window-system nil)
                    'eclipse)
                   ((string-equal (getenv "GTK_THEME") "Adwaita:dark")
                    'modus-vivendi)
                   (t 'modus-operandi))))
  (condition-case nil
      (load-theme theme t)
    (error
      (package-install 'spacemacs-theme)
      (load-theme theme t))))

(use-package almost-mono-themes
  :defer t
  :ensure t)
(use-package atom-one-dark-theme
  :defer t
  :ensure t)
(use-package vscode-dark-plus-theme
  :defer t
  :ensure t)
(use-package eclipse-theme
  :defer t
  :ensure t)
(use-package modus-themes
  :defer t
  :ensure t)

;; Flyspell options
(use-package ispell
  :ensure t
  :commands ispell
  :bind (("<f8>" . fd-switch-dictionary))
  :config (progn
            (add-to-list 'ispell-dictionary-alist
                         '("de"
                           "[a-zA-Z\304\326\334\344\366\337\374]"
                           "[^a-zA-Z\304\326\334\344\366\337\374]"
                           "[']" t ("-C" "-d" "de_DE") "~latin1" iso-8859-15))
            (defun fd-switch-dictionary()
              (interactive)
              (let* ((dic ispell-current-dictionary)
                     (change (if (string= dic "de") "english" "de")))
                (ispell-change-dictionary change)
                (message "Dictionary switched from %s to %s" dic change)))
            (setq ispell-program-name "aspell"
                  ispell-list-command "list"
                  ispell-extra-args '("--sug-mode=fast"))))

(use-package flyspell
  :after ispell
  :commands flyspell-mode
  :ensure t
  :config (setq flyspell-issue-message-flag nil))

;; Term mode
;; enable cua and transient mark modes in term-line-mode
(defadvice term-line-mode (after term-line-mode-fixes ())
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) t)
  (local-set-key "\C-c\C-n" 'term-char-mode))
(ad-activate 'term-line-mode)
;; disable cua and transient mark modes in term-char-mode
(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil))
(ad-activate 'term-char-mode)


;; Tramp
(use-package tramp :ensure t)

;; Interactively Do Things
(use-package ido
  :ensure t
  :bind (("C-." . ido-goto-symbol))
  :config (progn
            (ido-mode t)

            (defun ido-goto-symbol (&optional symbol-list)
              "Refresh imenu and jump to a place in the buffer using Ido."
              (interactive)
              (unless (featurep 'imenu)
                (require 'imenu nil t))
              (cond
               ((not symbol-list)
                (let ((ido-mode ido-mode)
                      (ido-enable-flex-matching
                       (if (boundp 'ido-enable-flex-matching)
                           ido-enable-flex-matching t))
                      name-and-pos symbol-names position)
                  (unless ido-mode
                    (ido-mode 1)
                    (setq ido-enable-flex-matching t))
                  (while (progn
                           (imenu--cleanup)
                           (setq imenu--index-alist nil)
                           (ido-goto-symbol (imenu--make-index-alist))
                           (setq selected-symbol
                                 (ido-completing-read "Symbol? " symbol-names))
                           (string= (car imenu--rescan-item) selected-symbol)))
                  (unless (and (boundp 'mark-active) mark-active)
                    (push-mark nil t nil))
                  (setq position (cdr (assoc selected-symbol name-and-pos)))
                  (cond
                   ((overlayp position)
                    (goto-char (overlay-start position)))
                   (t
                    (goto-char position)))))
               ((listp symbol-list)
                (dolist (symbol symbol-list)
                  (let (name position)
                    (cond
                     ((and (listp symbol) (imenu--subalist-p symbol))
                      (ido-goto-symbol symbol))
                     ((listp symbol)
                      (setq name (car symbol))
                      (setq position (cdr symbol)))
                     ((stringp symbol)
                      (setq name symbol)
                      (setq position
                            (get-text-property 1 'org-imenu-marker symbol))))
                    (unless (or (null position) (null name)
                                (string= (car imenu--rescan-item) name))
                      (add-to-list 'symbol-names name)
                      (add-to-list 'name-and-pos (cons name position))))))))))

(use-package erefactor
  :defer t
  :ensure t)

(use-package esup
  :commands esup
  :ensure t)

(use-package dumb-jump
  :ensure t
  :config (progn
            (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

            (defhydra dumb-jump-hydra (:color blue :columns 3)
              "Dumb Jump"
              ("j" dumb-jump-go "Go")
              ("o" dumb-jump-go-other-window "Other window")
              ("e" dumb-jump-go-prefer-external "Go external")
              ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
              ("i" dumb-jump-go-prompt "Prompt")
              ("l" dumb-jump-quick-look "Quick look")
              ("b" dumb-jump-back "Back"))))

(use-package jsonnet-mode
  :mode ("\\.jsonnet$")
  :ensure t)

;; local lisp code
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(use-package sudo-save)

(use-package redo+
  :demand t
  :bind (("C--" . redo)))

(use-package symon
  :commands symon-mode
  :defer t)

(use-package sx
  :ensure t
  :config (bind-keys :prefix "C-c s"
                     :prefix-map my-sx-map
                     :prefix-docstring "Global keymap for SX."
                     ("q" . sx-tab-all-questions)
                     ("i" . sx-inbox)
                     ("o" . sx-open-link)
                     ("u" . sx-tab-unanswered-my-tags)
                     ("a" . sx-ask)
                     ("s" . sx-search)))

(use-package narrow-indirect
  :bind (:map ctl-x-4-map
              ("nd" . ni-narrow-to-defun-indirect-other-window)
              ("nn" . ni-narrow-to-region-indirect-other-window)
              ("np" . ni-narrow-to-page-indirect-other-window)))

(use-package visual-fill-column
  :ensure t)

(use-package cmake-mode
  :mode ("CMakeLists\\.txt$" "\\.cmake$")
  :commands cmake-mode)

(use-package deadgrep
  :commands (rg deadgrep)
  :ensure t
  :config (defun rg ()
            (interactive)
            (call-interactively #'deadgrep)))

(use-package pypytrace-mode
  :defer t
  :commands pypytrace-mode)

(use-package kickasm-mode
  :defer t
  :commands kickasm-mode
  :config (setq
           kickasm-c64debugger-command (expand-file-name
                                        "~/.emacs.d/c64debugger/c64debugger -autojmp -wait 4000")
           kickasm-vice-command "x64")
  :init (add-hook 'kickasm-mode-hook
                  (lambda () (add-hook 'before-save-hook
                                       (lambda ()
                                         (whitespace-cleanup)
                                         (indent-region (point-min) (point-max) nil)
                                         (untabify (point-min) (point-max)))
                                       nil
                                       'local))))

(use-package emms
  :ensure t
  :defer t
  :if (not (eq system-type 'windows-nt))
  :config (progn
            (require 'emms-setup)
            (emms-all)
            (emms-default-players)
            (add-to-list 'emms-info-functions 'emms-info-exiftool t)
            (setq emms-source-file-default-directory "~/Music/")))

(use-package javap-handler)

(use-package filladapt
  :ensure t
  :config (add-hook 'c-mode-common-hook
	            (lambda ()
	              (when (featurep 'filladapt)
	                (c-setup-filladapt)))))

(use-package flymake
  :defer t
  :after lsp-mode
  :config (progn
           (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
           (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)))

(if (not (eq system-type 'windows-nt))
    (progn
      (add-to-list 'load-path (locate-user-emacs-file "emacs-secondmate/emacs"))
      (use-package secondmate
        :defer t
        :bind (("C-c M-/" . secondmate))
        :commands secondmate
        :config (setq secondmate-url "https://lively-kernel.org/swacopilot"))))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :if (memq window-system '(mac ns x pgtk))
  :config (exec-path-from-shell-initialize))

(use-package rustic
  :ensure t
  :defer t
  :mode ("\\.rs$")
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))
            (setq rustic-lsp-client 'lsp-mode
                  rustic-lsp-server
                  lsp-rust-server)))

(use-package multiple-cursors
  :ensure t
  :defer t)

(use-package adaptive-wrap
  :ensure t
  :defer t)

(use-package emojify
  :ensure t
  :commands emojify-insert-emoji
  :config (progn
            (when (member "Segoe UI Emoji" (font-family-list))
              (set-fontset-font
               t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))))

(use-package github-explorer
  :defer t
  :ensure t)

(use-package quelpa
  :ensure t
  :defer t
  :commands copilot-mode
  :config (progn
            (quelpa
             '(copilot
               :fetcher git
               :url "https://github.com/zerolfx/copilot.el"
               :branch "main"
               :files ("dist" "*.el")))
            (setq copilot-idle-delay 1
                  copilot-log-max 100)
            (require 'copilot)

            ;; Based on function from https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
            (defun copilot-complete-or-accept ()
              "Command that either triggers a completion or accepts one if one is available."
              (interactive)
              (if (copilot--overlay-visible)
                  (progn
                    (copilot-accept-completion))
                (copilot-complete)))
            (define-key copilot-mode-map (kbd "C-<return>") #'copilot-complete-or-accept)))

;; (add-to-list 'load-path (locate-user-emacs-file "jsonnet-language-server/editor/emacs"))
;; (require 'jsonnet-language-server)

(defun my/webkit-visit-alternate ()
  (interactive)
  (let ((old-val webkit-browse-url-force-new))
    (setq webkit-browse-url-force-new nil)
    (call-interactively 'webkit)
    (setq webkit-browse-url-force-new old-val)))

(defun my/webkit-reload-with-proxy ()
  (interactive)
  (if (and url-proxy-services
           (local-variable-p 'my/webkit-proxy)
           (buffer-local-value 'my/webkit-proxy (current-buffer)))
      (progn
        (webkit--proxy-set-default webkit--id)
        (setq-local my/webkit-proxy nil))
    (progn
      (setq-local my/webkit-proxy (cdar url-proxy-services))
      (webkit--proxy-set-uri webkit--id (format "http://%s" (cdar url-proxy-services)))))
  (run-with-timer 2 nil (lambda (id) (webkit--reload id)) webkit--id))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun webkit ()
  (interactive)
  (add-to-list 'load-path (locate-user-emacs-file "emacs-webkit"))

  ;; If you don't care so much about privacy and want to give your data to google
  (setq webkit-search-prefix "https://google.com/search?q=") 

  ;; Specify a different set of characters use in the link hints
  ;; For example the following are more convienent if you use dvorak
  (setq webkit-ace-chars "1234567890")

  ;; If you want history saved in a different place or
  ;; Set to `nil' to if you don't want history saved to file (will stay in memory)
  ;; (setq webkit-history-file "~/path/to/webkit-history") 

  ;; If you want cookies saved in a different place or
  ;; Set to `nil' to if you don't want cookies saved
  ;; (setq webkit-cookie-file "~/path/to/cookies")

  ;; See the above explination in the Background section
  ;; This must be set before webkit.el is loaded so certain hooks aren't installed
  ;; (setq webkit-own-window t) 

  ;; Set webkit as the default browse-url browser
  ;; (setq browse-url-browser-function 'webkit-browse-url)

  ;; Force webkit to always open a new session instead of reusing a current one
  (setq webkit-browse-url-force-new t)

  ;; Globally disable javascript
  ;; (add-hook 'webkit-new-hook #'webkit-enable-javascript)

  ;; Override the "loading:" mode line indicator with an icon from `all-the-icons.el'
  ;; You could also use a unicode icon like ↺
  (defun webkit--display-progress (progress)
    (setq webkit--progress-formatted
          (if (equal progress 100.0)
              ""
            (format "%s%.0f%%  " (all-the-icons-faicon "spinner") progress)))
    (force-mode-line-update))

  ;; Set action to be taken on a download request. Predefined actions are
  ;; `webkit-download-default', `webkit-download-save', and `webkit-download-open'
  ;; where the save function saves to the download directory, the open function
  ;; opens in a temp buffer and the default function interactively prompts.
  (setq webkit-download-action-alist '(("\\.pdf\\'" . webkit-download-open)
                                       ("\\.png\\'" . webkit-download-save)
                                       (".*" . webkit-download-default)))

  ;; Globally use a proxy
  ;; (add-hook 'webkit-new-hook (lambda () (webkit-set-proxy "socks://localhost:8000")))

  ;; Globally use the simple dark mode
  ;; (setq webkit-dark-mode t)

  (require 'org)
  (require 'ol)
  (require 'webkit)
  (require 'webkit-ace) ;; If you want link hinting
  ;; (require 'webkit-dark) ;; If you want to use the simple dark mode

  (define-key webkit-mode-map (kbd "C-x C-v") #'my/webkit-visit-alternate)
  (define-key webkit-mode-map (kbd "C-x r p") #'my/webkit-reload-with-proxy)

  (call-interactively 'webkit))
