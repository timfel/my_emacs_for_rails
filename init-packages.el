;;; -*- lexical-binding: t -*-
(package-initialize)
;; (setq use-package-compute-statistics t)
(require 'compile)

(setq warning-minimum-level :error
      use-package-verbose t)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("cselpa" . "https://elpa.thecybershadow.net/packages/"))

(condition-case nil
    (require 'use-package)
  (file-error
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(use-package use-package-ensure-system-package)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(use-package dash
  :ensure t)

(use-package timfel
  :after dash
  :demand t)

(use-package ht
  :defer t
  :ensure t)

(use-package isearch
  :bind (("C-S-s" . timfel/isearch-word-at-point)
	 :map isearch-mode-map
	 ([backspace] . isearch-edit-string))
  :hook ((isearch-mode . timfel/isearch-yank-word-hook))
  :config
  (defun timfel/isearch-word-at-point ()
    (interactive)
    (call-interactively 'isearch-forward-regexp))

  (defun timfel/isearch-yank-word-hook ()
    (when (equal this-command 'timfel/isearch-word-at-point)
      (let ((string (concat "\\<"
                            (buffer-substring-no-properties
                             (progn (skip-syntax-backward "w_") (point))
                             (progn (skip-syntax-forward "w_") (point)))
                            "\\>")))
	(skip-syntax-backward "w_") ;; go before the current search term
	(if (and isearch-case-fold-search
		 (eq 'not-yanks search-upper-case))
            (setq string (downcase string)))
	(setq string (regexp-quote (substring string 2 (- (length string) 2))))
	(setq isearch-string string
              isearch-message
              (concat isearch-message
                      (mapconcat 'isearch-text-char-description
				 string ""))
              isearch-yank-flag t)
	(isearch-search-and-update)))))

(use-package python-mode
  :ensure python
  :mode ("\\.py$" "\\.pyi$" "\\.pyx$")
  :hook ((python-mode . turn-on-font-lock)
         (python-mode . timfel/friendly-whitespace)
	 (python-mode . (lambda ()
			  (setq ac-sources
				'(ac-source-python
				  ac-source-semantic
				  ac-source-words-in-same-mode-buffers
				  ac-source-yasnippet
				  ac-source-abbrev))))))

(use-package xref
  :config
  (if (and
     (not (fboundp 'xref-quit-and-goto-xref))
     (fboundp 'xref-goto-xref))
    (defun xref-quit-and-goto-xref ()
      "Quit *xref* buffer, then jump to xref on current line."
      (interactive)
      (let* ((buffer (current-buffer))
             (xref (or (xref--item-at-point)
                       (user-error "No reference at point")))
             (xref--current-item xref))
        (quit-window nil nil)
        (xref--show-location (xref-item-location xref) t)
        (next-error-found buffer (current-buffer))))))

(use-package hippie-exp
  :config
  (defun timfel/try-complete-abbrev (old)
    (if (expand-abbrev) t nil))
  (setq hippie-expand-try-functions-list
	'(timfel/try-complete-abbrev
          try-complete-file-name
          try-expand-dabbrev)))

;; additional modes I like
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" "\\.yaml$" "Gemfile.lock$"))

(use-package haml-mode :ensure t
  :mode ("\\.haml$"))

(use-package sass-mode :ensure t
  :mode ("\\.sass$"))

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

(use-package ruby-electric :ensure t
  :defer t
  :after ruby-mode)

(use-package ruby-mode :ensure t
  :mode ("\\.rb$" "\\.rjs$" "\\.rake$" "Rakefile$" "Gemfile$" "Vagrantfile$")
  :hook ((ruby-mode . turn-on-font-lock)
         (ruby-mode . ruby-electric-mode)
         (ruby-mode . (lambda() (progn
                                  (set (make-local-variable 'indent-tabs-mode) 'nil)
                                  (set (make-local-variable 'tab-width) 2)))))
  :config (progn
            ;; Patch ruby-mode
            (defun ruby-accurate-end-of-block (&optional end)
              "(tfel): Fixes an issue I had with ruby-mode."
              (let (state
                    (end (or end (point-max))))
                (while (and (setq state (apply 'ruby-parse-partial end state))
                            (nth 2 state) (>= (nth 2 state) 0) (< (point) end)))))))

(setq cloud-storage
      (cl-case system-type
            (windows-nt "//nx89384.your-storageshare.de@SSL/DavWWWRoot/remote.php/dav/files/timfelgentreff/")
            (t (expand-file-name "~/CloudDrive"))))

(use-package org
  :commands org-mode
  :mode (("\\.org$" . org-mode))
  :ensure org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c <right>" . org-shiftright)
         ("C-c <left>" . org-shiftleft)
         ("C-c M-RET" . org-insert-subheading))
  :config (progn
            (org-babel-do-load-languages
             'org-babel-load-languages '((C . t) (shell . t) (python t) (ruby . t) (js . t)))

            (setq org-log-done 'time)
            (require 'org-tempo)
            (let* ((todos (expand-file-name "SyncFolder/todo.org" cloud-storage))
                   (notes (expand-file-name "SyncFolder/notes.org" cloud-storage)))
              (setq
               org-return-follows-link t
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

(use-package ox-gfm
  :ensure t
  :after org)

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
             org-download-screenshot-method (if (eq system-type 'windows-nt)
                                                (format "%s %s"
                                                        (executable-find "python3")
                                                        (expand-file-name "~/dotfiles/bin/wslscr.py %s"))
                                              (expand-file-name "~/bin/wslscr.py %s")))
            (set-default 'org-download-image-dir (expand-file-name "Screenshots/" cloud-storage))))

(use-package hide-mode-line
  :ensure t
  :after org-present)

(use-package org-present
  :ensure t
  :after (org visual-fill-column)
  :bind (:map org-mode-map
              ("<f12>" . (lambda ()
                           (interactive)
                           (delete-other-windows)
                           (toggle-frame-fullscreen)
                           (if org-present-mode
                               (org-present-quit)
                             (org-present)))))
  :config (progn
            ;; Load org-faces to make sure we can set appropriate faces
            (require 'org-faces)

            ;; Resize Org headings
            (dolist (face '((org-level-1 . 1.2)
                            (org-level-2 . 1.1)
                            (org-level-3 . 1.05)
                            (org-level-4 . 1.0)
                            (org-level-5 . 1.1)
                            (org-level-6 . 1.1)
                            (org-level-7 . 1.1)
                            (org-level-8 . 1.1)))
              (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))

            ;; Make the document title a bit bigger
            (set-face-attribute 'org-document-title nil :weight 'bold :height 1.3)

            ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
            ;; This can be globally set
            (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
            (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
            (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
            (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
            (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

            (setq org-present-startup-folded nil
                  org-present-text-scale 2
                  org-present-cursor-cache nil))
  :hook ((org-present-mode . (lambda ()
                               ;; Hide emphasis markers on formatted text
                               (setq org-hide-emphasis-markers t)
                               ;; Set a blank header line string to create blank space at the top
                               (setq header-line-format " ")
                               ;; bit more space between lines
                               (setq line-spacing 0.5)
                               ;; Load theme
                               (load-theme 'spacemacs-dark t)
                               ;; non-transparent emacs
                               (set-frame-parameter (selected-frame) 'alpha '(96 . 100))
                               ;; Tweak font sizes
                               (setq-local face-remapping-alist '((default (:height 3.0) variable-pitch)
                                     (header-line (:height 8.0) variable-pitch)
                                     (org-document-title (:height 3.5) org-document-title)
                                     (org-code (:height 3.1) fixed-pitch)
                                     (org-verbatim (:height 3.1) org-verbatim)
                                     (org-block (:height 2.5) org-block)
                                     (org-block-begin-line (:height 1.4) org-block)))
                               ;; remove menubar
                               (menu-bar-mode 0)
                               (hide-mode-line-mode 1)
                               (org-present-big)
                               (org-display-inline-images)
                               (internal-show-cursor (selected-window) nil)
                               (setq buffer-read-only t)
                               ;; variable sized fonts
                               (variable-pitch-mode 1)
                               ;; center document
                               (setq fill-column 57
                                     visual-fill-column-width 40
                                     visual-fill-column-adjust-for-text-scale t
                                     visual-fill-column-center-text t)
                               (visual-fill-column-mode 1)
                               (visual-line-mode 1)))
         (org-present-mode-quit . (lambda ()
                                    ;; Hide emphasis markers on formatted text
                                    (setq org-hide-emphasis-markers nil)
                                    ;; reset header
                                    (setq header-line-format nil)
                                    ;; reset line spacing
                                    (setq line-spacing nil)
                                    ;; reset theme
                                    (my/load-default-theme)
                                    ;; non-transparent emacs
                                    (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
                                    ;; reset font sizes
                                    (setq-local face-remapping-alist nil)
                                    ;; re-add menu
                                    (menu-bar-mode 1)
                                    (hide-mode-line-mode 0)
                                    (org-present-small)
                                    (internal-show-cursor (selected-window) t)
                                    (setq buffer-read-only nil)
                                    ;; code sized fonts
                                    (variable-pitch-mode 0)
                                    ;; stop centering document
                                    (setq visual-fill-column-width 110
                                          visual-fill-column-adjust-for-text-scale t
                                          visual-fill-column-center-text nil)
                                    (visual-fill-column-mode 0)
                                    (setq visual-fill-column-center-text nil)
                                    (visual-line-mode 0)))))

(use-package projectile
  :ensure t
  :defer t
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
  :after fuzzy
  :defer t
  :config
  (setq helm-buffers-maybe-switch-to-tab nil))

(use-package imenu
  :bind (("C-." . (lambda ()
                    (interactive)
                    (imenu--cleanup)
                    (imenu--menubar-select imenu--rescan-item)
                    (imenu (imenu-choose-buffer-index))))))

(use-package icomplete
  :config
  (add-to-list 'completion-category-overrides '(project-file (styles initials flex)))
  (add-to-list 'completion-category-overrides '(imenu (styles flex))))

(use-package grep
  :config
  (add-to-list 'grep-find-ignored-directories "mxbuild")
  (add-to-list 'grep-find-ignored-directories "site-packages"))

(use-package project
  :bind (("C-t" . project-or-external-find-file)))

(use-package code-workspace
  :load-path "~/.emacs.d/lisp/"
  :after project
  :demand t)

;; Auto completion
(use-package yasnippet
  :ensure t
  :hook ((lsp-mode . yas-minor-mode)
         (python-mode . yas-minor-mode)))

(use-package company
  :ensure t
  :bind (("M-?" . company-complete))
  :config (progn
            (global-company-mode t)
            (setq
             company-dabbrev-downcase 0
             company-idle-delay (if (eq system-type 'windows-nt) 10 0.2))))

(use-package company-box
  :ensure t
  :after company
  :if (window-system)
  :hook (company-mode . company-box-mode))

(use-package diff
  :after vc
  :bind (:map diff-mode-map
         ("c" . vc-next-action)))

(use-package vc
  :if (eq system-type 'windows-nt)
  :config
  (setq vc-revert-show-diff nil)
  :bind (("C-x C-z" . project-vc-dir)))

(use-package vc-dir
  :if (eq system-type 'windows-nt)
  :bind (:map vc-dir-mode-map
         ("!" . eshell)
         ("F" . vc-pull)
         ("P" . (lambda ()
                  (interactive)
                  (if (eq 'Git (vc-deduce-backend))
                      (vc-git-push t)
                    (vc-push))))
         ("k" . (lambda ()
                  (interactive)
                  (let* ((files (or (vc-dir-marked-files)
                                    (list (vc-dir-current-file))))
                         (tracked
                          (seq-filter (lambda (file)
                                        (not (eq (vc-call-backend vc-dir-backend 'state file)
                                                 'unregistered)))
                                      files)))
                    (map-y-or-n-p "Revert %s? " #'vc-revert-file tracked)
                    (map-y-or-n-p "Delete %s? " #'delete-file files)
                    (revert-buffer))))
         ("TAB" . (lambda ()
                    (interactive)
                    (vc-diff nil nil (list (vc-deduce-backend) (list (vc-dir-current-file)) nil nil))))
         ("c" . vc-next-action)
         ("i" . vc-dir-ignore)
         ("g" . (lambda ()
                  (interactive)
                  (vc-dir-hide-up-to-date)
                  (revert-buffer)
                  (run-with-idle-timer 4 nil #'vc-dir-hide-up-to-date)))
         ("U" . (lambda ()
                  (interactive)
                  (dolist (file (vc-dir-marked-files))
                    (vc-dir-mark-by-regexp (regexp-quote (file-relative-name file (vc-root-dir))) t))))
         ("s" . (lambda ()
                  (interactive)
                  (let* ((backend (vc-deduce-backend))
                         (file (vc-dir-current-file))
                         (fileset (list backend (list file) nil nil nil)))
                    (condition-case nil (vc-register fileset) (error nil))
                    (vc-dir-mark-by-regexp (regexp-quote (file-relative-name file (vc-root-dir))) nil))))
         ("u" . (lambda ()
                  (interactive)
                  (let* ((backend (vc-deduce-backend))
                         (file (vc-dir-current-file))
                         (fileset (list backend (list file) nil nil nil)))
                    (if (eq (vc-state file) 'added) (vc-revert-file file))
                    (vc-dir-mark-by-regexp (regexp-quote (file-relative-name file (vc-root-dir))) t))))))

(use-package vc-git
  :if (eq system-type 'windows-nt)
  :bind (:map vc-git-log-edit-mode-map
         ("C-c C-a" . vc-git-log-edit-toggle-amend)
         ("C-c C-l" . vc-print-log)
         :map vc-git-log-view-mode-map
         ("v" . (lambda ()
                  (interactive)
                  (let* ((rev (log-view-current-entry))
                         (default-directory (vc-root-dir))
                         (cmd (format "%s revert --no-commit %s" vc-git-program (cadr rev))))
                    (if (yes-or-no-p (concat "Run `" cmd "`?"))
                        (shell-command cmd)))))
         ("r" . (lambda ()
                  (interactive)
                  (let* ((rev (log-view-current-entry))
                         (default-directory (vc-root-dir))
                         (cmd (format "%s rebase --allow-empty --autostash --autosquash %s" vc-git-program (cadr rev))))
                    (if (yes-or-no-p (concat "Run `" cmd "`?"))
                        (shell-command cmd)))))))

(use-package magit
  :if (not (eq system-type 'windows-nt))
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
                  ;; do not show diff when committing
                  (remove-hook 'server-switch-hook 'magit-commit-diff)
                  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
                  ;; reduce the number of things in the status buffer to reduce calls
                  (setq magit-refresh-status-buffer nil)
                  (mapcar (lambda (x) (remove-hook 'magit-status-sections-hook x))
                          (list 'magit-insert-am-sequence
                                'magit-insert-rebase-sequence
                                'magit-insert-sequencer-sequence
                                'magit-insert-bisect-output
                                'magit-insert-bisect-rest
                                'magit-insert-bisect-log
                                'magit-insert-merge-log
                                'magit-insert-stashes
                                'magit-insert-status-headers
                                'magit-insert-tags-header
                                'magit-insert-unpushed-to-pushremote
                                'magit-insert-unpushed-to-upstream-or-recent
                                'magit-insert-unpulled-from-pushremote
                                'magit-insert-unpulled-from-upstream
                                ))
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

(use-package popup
  :defer t
  :ensure t)

(use-package fuzzy
  :defer t
  :ensure t)

(use-package pcache
  :defer t
  :ensure t)

(use-package logito
  :defer t
  :ensure t)

(use-package all-the-icons
  :demand t
  :ensure t)

(use-package all-the-icons-completion
  :after all-the-icons
  :if (display-graphic-p)
  :demand t
  :ensure t)

(use-package all-the-icons-dired
  :after all-the-icons
  :if (display-graphic-p)
  :demand t
  :ensure t)

(use-package doom-modeline
  ;; remember to run (all-the-icons-install-fonts) manually some time
  :demand t
  :if (not (eq system-type 'windows-nt))
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
(use-package auctex-mode
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

(use-package reftex-mode
  :after auctex-mode
  :ensure reftex
  :mode ("\\.bib$")
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

(use-package treemacs
  :ensure t
  :defer t
  :bind (("<f5>" . (lambda ()
		     (interactive)
		     (require 'treemacs)
		     (treemacs--restore)
		     (require 'lsp-java)
		     (let* ((cwd (expand-file-name "."))
			    (path (completing-read (format "Workspace or folder (return for %s): " cwd)
						   (completion-table-dynamic
						    (lambda (s)
						      (let* ((parent-folder (file-name-directory s))
							     (folders (if (and parent-folder (file-directory-p parent-folder))
									  (seq-filter
									   (lambda (p) (file-directory-p p))
									   (seq-map
									    (lambda (d) (file-name-concat parent-folder d))
									    (directory-files parent-folder)))))
							     (workspaces (seq-map (lambda (elt) (treemacs-workspace->name elt))
										  (treemacs-workspaces))))
							(if (and folders (string-prefix-p s cwd))
							    (setq folders (seq-concatenate 'list folders (list cwd))))
							(if (string-empty-p s)
							    (setq workspaces (seq-concatenate 'list workspaces (list cwd))))
							(if folders
							    folders
							  workspaces)))))))
		       (if (string-empty-p path)
			   (setq path cwd))
		       (let* ((name (replace-regexp-in-string "[^a-zA-Z0-9]" "_" path))
			      (ws (or (treemacs-find-workspace-by-name path) (treemacs-find-workspace-by-name name))))
			 (if (and (file-directory-p path) (file-name-absolute-p path))
			     (progn
			       (if (not ws)
				   (setq ws (nth 1 (treemacs-do-create-workspace name))))
			       (if (not (seq-find (lambda (elt)
						    (or (string-equal-ignore-case (treemacs-project->name elt) path)
							(string-equal-ignore-case (treemacs-project->path elt) path)))
						  (treemacs-workspace->projects ws)))
				   (let ((projects (treemacs-workspace->projects ws)))
				     (push (treemacs-project->create! :name path :path path :path-status 'local-readable)
					   projects)))))
			 (if ws
			     (progn
			       (treemacs-do-switch-workspace ws)
			       (treemacs-select-window))))))))
  :config (progn
            (require 'desktop)

            (setq treemacs-git-mode nil)

            ;; Sessions
            (defun my/treemacs-desktop-hook ()
              (dolist (buffer (buffer-list))
                (let ((name (buffer-name buffer)))
                  (if (and (not (string-match-p "\\*" name))
                           (not (buffer-modified-p buffer))
                           (not (get-buffer-window (current-buffer) t)))
                      (kill-buffer buffer))))
              (if (and desktop-save-mode desktop-dirname)
                  (desktop-save desktop-dirname))
              (desktop-save-mode-off)
              (setq
               desktop-base-file-name
               (concat (treemacs-workspace->name (treemacs-current-workspace))
                       ".desktop")
               desktop-base-lock-name
               (concat (treemacs-workspace->name (treemacs-current-workspace))
                       ".emacs.desktop.lock"))
              (desktop-read)
              (desktop-save-mode 1))
            (add-hook 'treemacs-switch-workspace-hook #'my/treemacs-desktop-hook)

            (setq
             history-length 10
             desktop-restore-eager 5
             desktop-auto-save-timeout 15
             desktop-buffers-not-to-save (concat "\\("
                                                 "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                                                 "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                                                 "\\)$"))
            (add-to-list 'desktop-globals-to-save 'file-name-history)
            (add-to-list 'desktop-modes-not-to-save 'dired-mode)
            (add-to-list 'desktop-modes-not-to-save 'Info-mode)
            (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
            (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
            (add-to-list 'desktop-modes-not-to-save 'grep-mode)
            (add-to-list 'desktop-modes-not-to-save 'magit-mode)
            (add-to-list 'desktop-modes-not-to-save 'treemacs-mode)
            (add-to-list 'desktop-modes-not-to-save 'deadgrep-mode)

            (setq treemacs-file-follow-delay 1.0
                  treemacs-width 45
                  treemacs-width-is-initially-locked t)))

(use-package treemacs-nerd-icons
  :if (not (display-graphic-p))
  :after treemacs
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package which-key
  :ensure t
  :config (progn
            (which-key-mode)
            (setq which-key-idle-delay 1.0)
            (which-key-setup-side-window-right-bottom)))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :preface (setq lsp-use-plists t)
  :ensure t
  :commands lsp
  :config (progn
            (require 'pcache)

            (defun lsp-goto-next-diagnostic ()
              "Get lsp-diagnostics, it returns a hash mapping file names to a list of
	 	hashes, each of which is a diagnostic. Search in the file names for the
	 	current buffer's file name. If found, search the list of diagnostics.
	 	Get the vallue for the :range key, and compare the :start of the
	 	resulting hash with the current point position until we find the next
	 	diagnostic that is after the current point. If found, set the point to
	 	that next diagnistic's start position. If there are no more diagnistics
	 	after the current point in the list, take the next file name from the
	 	outer hash. If the file name that got picked is not the current buffer,
	 	open the file and position the point at the start of the range of the
	 	first hash in the list of diagnistics."
              (interactive)
              (let* ((current-file (or (buffer-file-name) (dired-get-file-for-visit)))
                     (diagnostics-table (lsp-diagnostics))
                     (all-files (hash-table-keys diagnostics-table))
                     (files-ordered (seq-sort #'string-lessp all-files))
                     (point-pos (point))
                     (found (gethash current-file diagnostics-table))
                     (files-to-seek (seq-drop-while
                                     (lambda (f) (and found (not (string-equal f current-file))))
                                     files-ordered)))
                (catch 'done
                  (dolist (file files-to-seek)
                    (let* ((diagnostics (gethash file diagnostics-table))
                           (next-diag
                            (seq-find (lambda (diag)
                                        (let ((range (plist-get diag :range))
                                              (severity (plist-get diag :severity)))
                                          (when (and range (< severity 2))
                                            (let* ((start (plist-get range :start))
                                                   (pos (lsp--line-character-to-point
                                                         (plist-get start :line)
                                                         (plist-get start :character))))
                                              (or
                                               (not (equal file current-file))
                                               (< point-pos pos))))))
                                      diagnostics)))
                      (when next-diag
                        (let* ((range (plist-get next-diag :range))
                               (start (plist-get range :start)))
                          (unless (equal file current-file)
                            (find-file file))
                          (goto-char (point-min))
                          (forward-line (plist-get start :line))
                          (forward-char (plist-get start :character))
                          (message "Jumped to diagnostic: %s (%d)" (plist-get next-diag :message) (plist-get next-diag :severity))
                          (throw 'done t))))))))

            (defun lsp-goto-previous-diagnostic ()
              "Move to the previous diagnostic before point."
              (interactive)
              (let* ((current-file (or (buffer-file-name) (dired-get-file-for-visit)))
                     (diagnostics-table (lsp-diagnostics))
                     (all-files (hash-table-keys diagnostics-table))
                     (files-ordered (seq-sort #'string-lessp all-files))
                     (point-pos (point))
                     (found (gethash current-file diagnostics-table))
                     (files-to-seek (seq-reverse (append (seq-take-while
                                                          (lambda (f) (and found (not (string-equal f current-file))))
                                                          files-ordered)
                                                         (list current-file)))))
                (catch 'done
                  (dolist (file files-to-seek)
                    (let* ((diagnostics (gethash file diagnostics-table))
                           (reversed-diags (reverse diagnostics))
                           (prev-diag
                            (seq-find (lambda (diag)
                                        (let ((range (plist-get diag :range))
                                              (severity (plist-get diag :severity)))
                                          (when (and range (< severity 2))
                                            (let* ((start (plist-get range :start))
                                                   (pos (lsp--line-character-to-point
                                                         (plist-get start :line)
                                                         (plist-get start :character))))
                                              (or
                                               (not (equal file current-file))
                                               (> point-pos pos))))))
                                      reversed-diags)))
                      (when prev-diag
                        (let* ((range (plist-get prev-diag :range))
                               (start (plist-get range :start))
                               (pos (lsp--line-character-to-point
                                     (plist-get start :line)
                                     (plist-get start :character))))
                          (unless (equal file current-file)
                            (find-file file))
                          (goto-char pos)
                          (message "Jumped to diagnostic: %s" (plist-get prev-diag :message))
                          (throw 'done t))))))))

            (defun my/c-clear-string-fences (orig-fun)
              (condition-case nil
                  (funcall orig-fun)
                (error
                 nil)))
            (advice-add #'c-clear-string-fences :around #'my/c-clear-string-fences)
            (advice-add #'c-restore-string-fences :around #'my/c-clear-string-fences)

            (if (executable-find "emacs-lsp-booster")
                (progn
                  (defun lsp-booster--advice-json-parse (old-fn &rest args)
                    "Try to parse bytecode instead of json."
                    (or
                     (when (equal (following-char) ?#)
                       (let ((bytecode (read (current-buffer))))
                         (when (byte-code-function-p bytecode)
                           (funcall bytecode))))
                     (apply old-fn args)))
                  (advice-add (if (progn (require 'json)
                                         (fboundp 'json-parse-buffer))
                                  'json-parse-buffer
                                'json-read)
                              :around
                              #'lsp-booster--advice-json-parse)

                  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
                    "Prepend emacs-lsp-booster command to lsp CMD."
                    (let ((orig-result (funcall old-fn cmd test?)))
                      (if (and (not test?)                             ;; for check lsp-server-present?
                               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                               lsp-use-plists
                               (not (functionp 'json-rpc-connection))  ;; native json-rpc
                               (executable-find "emacs-lsp-booster"))
                          (progn
                            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
                              (setcar orig-result command-from-exec-path))
                            (message "Using emacs-lsp-booster for %s!" orig-result)
                            (cons "emacs-lsp-booster" orig-result))
                        orig-result)))
                  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)))

            (defun my/advice-vscode-workspace-load (&rest args)
              (interactive)
              (dolist (folder (lsp-session-folders (lsp-session)))
                (let ((folders (gethash 'jdtls (lsp-session-server-id->folders (lsp-session)))))
                  (if (not (seq-contains-p folders folder))
                      (puthash 'jdtls
                               (append (gethash 'jdtls
                                                (lsp-session-server-id->folders (lsp-session)))
                                       (list folder))
                               (lsp-session-server-id->folders (lsp-session)))))))

            (advice-remove 'lsp-load-vscode-workspace #'my/advice-vscode-workspace-load)

            (defun my/lsp-notify-changed-file ()
              (interactive)
              (if-let ((workspace (car (gethash (lsp-workspace-root) (lsp-session-folder->servers (lsp-session))))))
                  (with-lsp-workspace workspace
                    (lsp-notify "workspace/didChangeWatchedFiles"
                                `((changes . [((type . ,(alist-get 'changed lsp--file-change-type))
                                               (uri . ,(lsp--path-to-uri buffer-file-name)))]))))))
            (setq lsp-print-io nil
                  lsp-headerline-arrow ">"
                  lsp-lens-enable t
                  lsp-completion-enable-additional-text-edit t
                  lsp-enable-snippet t
                  lsp-enable-indentation nil
                  lsp-before-save-edits t
                  lsp-enable-file-watchers nil)))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :after lsp-mode
  :bind (:map lsp-mode-map
         ("C-." . helm-semantic-or-imenu)
         ("C-S-t" . helm-lsp-workspace-symbol)
         ("C-," . lsp-execute-code-action)
         ("C-c f" . flycheck-list-errors)
         :map lsp-ui-mode-map
         ("C-c e" . lsp-treemacs-errors-list)
         ("M-," . xref-pop-marker-stack)
         ("M-." . lsp-ui-peek-find-definitions)
         ("C-M-." . lsp-ui-peek-find-references))
  :config (progn
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
                  lsp-completion-show-detail t
                  lsp-completion-show-kind nil
                  lsp-completion-sort-initial-results nil
                  lsp-response-timeout 30
                  lsp-diagnostic-clean-after-change nil
                  lsp-eldoc-render-all t
                  lsp-ui-peek-always-show t
                  lsp-ui-doc-enable (display-graphic-p)
                  lsp-ui-doc-max-height 30
                  lsp-ui-doc-position 'top
                  lsp-ui-doc-use-webkit (and (display-graphic-p) (not (eq system-type 'windows-nt)))
                  lsp-ui-doc-show-with-cursor t
                  lsp-ui-sideline-enable (not (eq system-type 'windows-nt))
                  lsp-ui-sideline-show-symbol nil
                  lsp-ui-sideline-show-hover (not (eq system-type 'windows-nt))
                  lsp-ui-sideline-show-code-actions t
                  lsp-ui-sideline-ignore-duplicate t
                  lsp-ui-sideline-delay 2
                  lsp-eldoc-enable-hover t
                  lsp-idle-delay 1.000
                  lsp-treemacs-error-list-current-project-only t
                  lsp-treemacs-error-list-expand-depth nil
                  lsp-treemacs-error-list-severity 1
                  lsp-tcp-connection-timeout 20
                  lsp-modeline-diagnostics-enable t
                  lsp-modeline-code-actions-enable nil
                  lsp-ui-sideline-code-actions-prefix ""
                  lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default
                  lsp-ui-sideline-update-mode 'line)))

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :config
  (cl-flet*
      ((error-list-advice (oldfun &rest args)
         "Show only diagnostics for the current folder if there are any."

         (if-let* ((root (determine-recent-project-root))
                   (folder-arg (seq-elt args 0))
                   (folder (expand-file-name folder-arg)))
             (when (string-prefix-p root folder)
               (apply oldfun args))
           (apply oldfun args))))

    (advice-add #'lsp-treemacs--build-error-list
                :around
                #'error-list-advice)))

(use-package cc-mode
  :hook ((cc-mode . infer-indentation-style)
	 (java-mode . timfel/friendly-whitespace)
	 (java-mode . (lambda () (company-mode t)))
	 (java-mode . (lambda ()
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
					    16)))))))
  :config
  (defun my/c-update-modeline (oldfun)
    ;; cc-mode assumes mode-line is a plain string at all times, see e.g.
    ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2018-07/msg00339.html
    ;;
    ;; The problem is that this is simply not always true with LSP and MMM in the
    ;; mix, so we get issues. I just advice the c-update-modeline function to
    ;; make mode-name a plain string
    (let ((mode-name (substring-no-properties (format-mode-line mode-name))))
      (funcall oldfun)))
  (advice-add #'c-update-modeline :around #'my/c-update-modeline)
  :bind (:map java-mode-map
	      ("C-S-o" . lsp-java-organize-imports)))

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
              (if-let* ((root (lsp-workspace-root (buffer-file-name)))
                        (cfg (f-join root "pyrightconfig.json")))
                (if (file-exists-p cfg) ; have a pyrightconfig.json, parse it
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

(use-package mmm-mode
  :ensure t
  :commands (mmm-parse-buffer)
  :hook (java-mode . (lambda () (mmm-parse-buffer)))
  :config (progn
            (require 'mmm-auto)
            (setq mmm-global-mode 'maybe)
            (defun polyglot-mode-match (front)
              (if (string-match "\"\\([[:word:]]+\\)\",[[:space:]]*\n?[[:space:]]*\"\"\"" front)
                  (let ((sym (intern-soft (concat (downcase (match-string-no-properties 1 front)) "-mode"))))
                    (if (fboundp sym) sym 'text-mode))
                (if (string-match "//[[:space:]]*language=\\([[:word:]]+\\)[[:space:]]*\n.+\"\"\"" front)
                    (let ((sym (intern-soft (concat (downcase (match-string-no-properties 1 front)) "-mode"))))
                      (if (fboundp sym) sym 'text-mode))
                'text-mode)))
            (mmm-add-classes
             '((java-text-block
                :match-submode polyglot-mode-match
                :front ".+\n.+\"\"\"$"
                :back ".*\"\"\".*"
                :face mmm-code-submode-face)))
            (mmm-add-classes
             '((md-javascript-block
                :submode javascript-mode
                :front "<script>"
                :back "</script>"
                :face mmm-code-submode-face)))
            (defun github-code-match (front)
              (if (string-match "```\\([[:word:]]+\\)" front)
                  (let ((sym (intern-soft (concat (downcase (match-string-no-properties 1 front)) "-mode"))))
                    (if (fboundp sym) sym 'fundamental-mode))
                'fundamental-mode))
            (mmm-add-classes
             '((md-github-code-block
                :match-submode github-code-match
                :front "```[a-z]+"
                :back "```"
                :face mmm-code-submode-face)))
            (mmm-add-mode-ext-class 'java-mode "\\.java$" 'java-text-block)
            (mmm-add-mode-ext-class 'markdown-mode "\\.md$" 'md-javascript-block)
            (mmm-add-mode-ext-class 'markdown-mode "\\.md$" 'md-github-code-block)))

(use-package koopa-mode
  :if (eq system-type 'windows-nt)
  :mode "\\.ps1\\'"
  :ensure t)

(use-package iedit
  :bind ("C-;" . iedit-mode)
  :ensure t)

(use-package lsp-java
  :ensure t
  :hook (java-mode . (lambda () (require 'lsp-java) (require 'dap-java)))
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
            (setq
             lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/snapshots/jdt-language-server-latest.tar.gz"
             lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true")
             lsp-java-content-provider-preferred "fernflower"
             lsp-java-save-actions-organize-imports t
             lsp-java-format-on-type-enabled nil
             lsp-java-format-comments-enabled t
             lsp-java-format-enabled t
             lsp-java-autobuild-enabled nil
             lsp-java-inhibit-message t
             lsp-java-import-gradle-enabled nil
             lsp-java-completion-import-order ["java" "javax" "org" "com"]
             lsp-java-import-order ["java" "javax" "org" "com"])

            (defun my/lsp-find-session-folder-with-mx (oldfun session file-name)
              (or (funcall oldfun session file-name)
                  (funcall oldfun session
                           (replace-regexp-in-string
                             "/mxbuild/\\(jdk[0-9]+/\\)?" "/"
                             file-name))))
            (advice-add #'lsp-find-session-folder :around #'my/lsp-find-session-folder-with-mx)

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
                       lsp-java-workspace-cache-dir (f-join lsp-java-workspace-dir "cache/")
                       lsp-session-file (expand-file-name (locate-user-emacs-file (format ".lsp-session-v1-%s" wsname)))))))
              (message (format "Setting Eclipse workspace to %s, session to %s" lsp-java-workspace-dir lsp-session-file))
              (message (format "You may have to adapt %s/.metadata/.plugins/org.eclipse.core.runtime/.settings/org.eclipse.jdt.launching.prefs to give the default VM the name that mx told you" lsp-java-workspace-dir))
              (find-file-noselect (format "%s/.metadata/.plugins/org.eclipse.core.runtime/.settings/org.eclipse.jdt.launching.prefs" lsp-java-workspace-dir))
              lsp-java-workspace-dir)

            (with-eval-after-load 'lsp-treemacs
              (add-hook 'treemacs-switch-workspace-hook #'lsp-java--treemacs-sync)
              (lsp-java--treemacs-sync))

            (defun lsp-if-jdtls-running ()
              (if (lsp-find-workspace 'jdtls nil) (lsp)))
            (add-hook 'java-mode-hook #'lsp-if-jdtls-running)))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :commands dap-debug
  ;; :bind (("C-c C-d" . my/dap-debug)
  ;;        :map java-mode-map
  ;;        ("C-c C-d" . my/dap-debug))
  ;; :hooks ((dap-session-created . dap-ui-repl)
  ;;         (dap-session-created . dap-ui-breakpoints))
  :config (progn
            (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
            (add-hook 'dap-terminated-hook (lambda (arg) (if (fboundp #'dap-hydra/nil) (call-interactively #'dap-hydra/nil))))

            (with-eval-after-load 'dap-python
              (dap-register-debug-template "DAP debugpy Attach 4711"
                                           (list :type "python"
                                                 :request "attach"
                                                 :connect (list :host "localhost"
                                                                :port 4711))))
            ;; default settings
            (setq
             dap-stack-trace-limit 40
             ;; dap-auto-configure-features '(sessions locals tooltip)
             dap-print-io nil
             ;; dap-auto-show-output t
             )
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

(use-package eclipse-theme
  :defer t
  :ensure t)

(use-package custom
  :demand t
  :config 
  (defun my/load-default-theme ()
    (if-let ((theme (cond ((eq window-system 'w32) 'eclipse)
			  ((eq window-system nil) 'eclipse)
			  (t 'eclipse))))
	(load-theme theme t)))
  (advice-add #'load-theme :before (lambda (&rest args)
                                     (mapcar #'disable-theme custom-enabled-themes)))
  (my/load-default-theme))

;; Flyspell options
(use-package ispell
  :defer t
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

(use-package tramp
  :defer 30
  :ensure t
  :config (progn
            (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
            (when (eq system-type 'windows-nt)
              (require 'cl-lib)
              ;; (assoc 'tramp-login-args (assoc "ssh" tramp-methods))
              (setq tramp-use-ssh-controlmaster-options nil)
              (cl-pushnew '("-tt")
                          (car (alist-get 'tramp-login-args
                                          (cdr (assoc "ssh" tramp-methods))))
                          :test #'equal))))

(use-package ido
  :commands (ido-find-file ido-switch-buffer)
  :bind (("C-x C-f" . ido-find-file)
         ("C-x b" . ido-switch-buffer))
  :ensure t
  :config (ido-mode t))

(use-package erefactor
  :defer t
  :ensure t)

(use-package esup
  :commands esup
  :ensure t)

(use-package dumb-jump
  :ensure t
  :defer 10
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
  :config (require 'lsp-jsonnet)
  :ensure t)

(use-package sudo-save
  :if (not (eq system-type 'windows-nt)))

(use-package term-keys
  :ensure t
  :if (not (display-graphic-p))
  :config
  (term-keys-mode t))

(use-package term
  :commands term
  :defer t
  :config
  ;; enable cua and transient mark modes in term-line-mode
  (advice-add #'term-line-mode :after (lambda (&rest args)
					(set (make-local-variable 'truncate-lines) nil)
					(set (make-local-variable 'cua-mode) nil)
					(set (make-local-variable 'transient-mark-mode) t)
					(local-set-key "\C-c\C-n" #'term-char-mode)))
  ;; disable cua and transient mark modes in term-char-mode
  (advice-add #'term-char-mode :after (lambda (&rest args)
					(set (make-local-variable 'truncate-lines) nil)
					(set (make-local-variable 'cua-mode) nil)
					(set (make-local-variable 'transient-mark-mode) nil))) 
  (advice-add #'term :after (lambda (&rest args)
                              (let ((b (get-buffer "*terminal*")))
                                (when b
                                  (text-scale-adjust 0)
                                  (text-scale-adjust -1)
                                  (call-interactively #'previous-buffer)
                                  (display-buffer-in-side-window
                                   b
                                   '((side . bottom)
                                     (slot . 1))))))))

(use-package redo+
  :bind (("C--" . redo)))

(use-package symon
  :commands symon-mode
  :defer t)

(use-package sx
  :ensure t
  :bind (:prefix "C-c s"
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
  :commands visual-fill-column-mode
  :ensure t)

(use-package cmake-mode
  :mode ("CMakeLists\\.txt$" "\\.cmake$")
  :commands cmake-mode)

(use-package deadgrep
  :commands (rg deadgrep)
  :ensure t
  :config (defun rg (what where)
            (interactive (list
                          (read-string
                           "Search what? "
                           (if (use-region-p)
                               (buffer-substring-no-properties (region-beginning) (region-end))
                             (if (symbol-at-point) (prin1-to-string (symbol-at-point)))))
                          (read-directory-name "Search where? ")))
            (deadgrep what where)))

(use-package pypytrace-mode
  :defer t
  :commands pypytrace-mode)

(use-package presentation
  :defer t
  :commands presentation-mode
  :config (progn

            (defun my/presentation-on-hook ()
              (menu-bar-mode 0))

            (defun my/presentation-off-hook ()
              (menu-bar-mode 1))

            (add-hook 'presentation-on-hook #'my/presentation-on-hook)
            (add-hook 'presentation-on-hook #'my/presentation-off-hook)))

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

(use-package typescript-mode
  :mode "\\.ts\\'"
  :ensure t)

(use-package filladapt
  :ensure t
  :hook ((c-mode-common . (lambda ()
			    (when (featurep 'filladapt)
                              (c-setup-filladapt))))))

(use-package flycheck
  :ensure t
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-prev-error)))

(use-package exec-path-from-shell
  :ensure t
  :defer 3
  :config
  (if (eq system-type 'windows-nt)
      (let* ((env-before (split-string (shell-command-to-string "powershell.exe -NoProfile -Command \"Get-ChildItem Env:* | ForEach-Object { \\\"$($_.Name)=$($_.Value)\\\" }\"") "\n" t))
             (env-before-alist (mapcar (lambda (s) (split-string s "=" t)) env-before))
             (output (shell-command-to-string "powershell.exe -NoProfile -Command \". $PROFILE; Write-Host $MyPath -NoNewline ; Write-Host ';' -NoNewline ; Write-Host $Env:PATH -NoNewline ; Write-Host '[ENV]' -NoNewline ; Get-ChildItem Env:* | ForEach-Object { \\\"$($_.Name)=$($_.Value)\\\" }\""))
             (parts (split-string output "\\[ENV\\]" t))
             (path-part (string-trim (car parts)))
             (env-after (split-string (cadr parts) "\n" t))
             (env-after-alist (mapcar (lambda (s) (split-string s "=" t)) env-after)))
        ;; Set PATH-related env vars
        (setenv "PATH" path-part)
        (setq exec-path (append (parse-colon-path path-part) (list exec-directory)))
        (setq-default eshell-path-env path-part)
        ;; Set all other environment variables that changed
        (dolist (pair env-after-alist)
          (let ((var (car pair))
                (val (mapconcat #'identity (cdr pair) "=")))
            (unless (string= val (or (cadr (assoc var env-before-alist)) ""))
              (setenv var val)))))
    (exec-path-from-shell-initialize)))

(use-package rustic
  :ensure t
  :defer t
  :mode ("\\.rs$" . rustic-mode)
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))
            (require 'rustic-lsp)
            (rustic-lsp-mode-setup)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-d" . mc/mark-more-like-this-extended)))

(use-package adaptive-wrap
  :ensure t
  :commands adaptive-wrap-prefix-mode
  :defer t)

(use-package emojify
  :ensure t
  :if (display-graphic-p)
  :commands emojify-insert-emoji
  :config (progn
            (set-fontset-font
             t
             'emoji
             (cond
              ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
              ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
              ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
              ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")  ; 🧗
              ((member "Symbola" (font-family-list)) "Symbola")))

            (set-fontset-font
             t
             'symbol
             (cond
              ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")
              ((member "Apple Symbols" (font-family-list)) "Apple Symbols")
              ((member "Symbola" (font-family-list)) "Symbola")))

            ;; nice on windows...
            (cond
             ((eq system-type 'windows-nt)
              (set-fontset-font t '(#x1F300 . #x1F5FF) "Segoe UI Symbol")))  ; 🔁, Miscellaneous Symbols and Pictographs

            ;; (add-hook 'java-mode-hook
            ;;           (lambda ()
            ;;             (setq prettify-symbols-alist
            ;;                   '(("@SuppressWarnings(\"unused\")" . ?🤷)))
            ;;             (prettify-symbols-mode 1)))

            (setq emojify-display-style 'unicode)
            (setq emojify-emoji-styles '(unicode))))

(use-package re-builder
  :commands re-builder
  :config (setq reb-re-syntax 'string))

(use-package gptel
  :ensure t
  :commands (gptel gptel-request)
  :config
  (setq gptel-model 'gemma3n:latest
        gptel-include-tool-results t
        gptel-include-reasoning t
        gptel-backend (gptel-make-ollama "Ollama"
                                         :host "localhost:11434"
                                         :stream t
                                         :models '(gemma3n:latest gemma3n-tools)))

  (setq
   cashpw/gptel-mode-line--indicator-querying "↑GPTEL↑ "
   cashpw/gptel-mode-line--indicator-responding "↓GPTEL↓ "
   cashpw/gptel-show-progress-in-mode-line t)
  (defun cashpw/gptel-mode-line--indicator (mode)
    "Return indicator string for MODE."
    (pcase mode
      ('querying
       cashpw/gptel-mode-line--indicator-querying)
      ('responding
       cashpw/gptel-mode-line--indicator-responding)
      (_
       "")))
  (defun cashpw/gptel-mode-line (command mode)
    "Update mode line to COMMAND (show|hide) indicator for MODE."
    (when cashpw/gptel-show-progress-in-mode-line
      (let ((indicator (list t (cashpw/gptel-mode-line--indicator mode))))
        (pcase command
          ('show
           (cl-pushnew indicator global-mode-string :test #'equal))
          ('hide
           (setf global-mode-string (remove indicator global-mode-string)))))
      (force-mode-line-update t)))
  (defun cashpw/gptel-mode-line--hide-all (&rest _)
    (cashpw/gptel-mode-line 'hide 'querying)
    (cashpw/gptel-mode-line 'hide 'responding))
  (defun cashpw/gptel-mode-line--show-querying ()
    (cashpw/gptel-mode-line--hide-all)
    (cashpw/gptel-mode-line 'show 'querying))
  (defun cashpw/gptel-mode-line--show-responding ()
    (cashpw/gptel-mode-line--hide-all)
    (cashpw/gptel-mode-line 'show 'responding))
  (add-hook 'gptel-post-request-hook 'cashpw/gptel-mode-line--show-querying)
  (add-hook 'gptel-pre-response-hook 'cashpw/gptel-mode-line--show-responding)
  (add-hook 'gptel-post-response-functions 'cashpw/gptel-mode-line--hide-all)

  (gptel-make-tool
   :function (lambda (command &optional working_dir)
               (with-temp-message (format "Executing command: `%s`" command)
                 (let ((default-directory (if (and working_dir (not (string= working_dir "")))
                                              (expand-file-name working_dir)
                                            default-directory)))
                   (shell-command-to-string command))))
   :name "execute_command"
   :description "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run."
   :args (list
          '(:name "command"
                  :type string
                  :description "The complete shell command to execute.")
          '(:name "working_dir"
                  :type string
                  :description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
   :category "command"
   :confirm t
   :include t)

  (gptel-make-tool
   :function (lambda (dir)
               (if (file-directory-p dir)
                   (setq default-directory dir)))
   :name "change_directory"
   :description "Change the default working directory for subsequent work."
   :args (list '(:name "dir" :type string :description "The directory to cd into."))
   :category "command"
   :confirm t
   :include nil)

  (gptel-make-tool
   :function (lambda () default-directory)
   :name "get_current_directory"
   :description "Return the name of the current working directory."
   :args (list)
   :confirm nil
   :include nil
   :category "command")

  (gptel-make-tool
   :name "get_recently_edited_filenames"
   :description "Return a list of the 5 most recently opened buffers in this emacs session to help better understand the context of what we are doing and the users request."
   :function (lambda ()
               (mapcar #'buffer-file-name
                       (seq-take
                        (delete-dups
                         (seq-remove
                          (lambda (b)
                            (or (null b)
                                (not (buffer-file-name b))
                                (string-prefix-p " " (buffer-name b))))
                          (buffer-list)))
                        5)))
   :args (list)
   :confirm nil
   :include nil
   :category "buffers")

  (gptel-make-tool
   :name "search_in_project"
   :description "Search for a string within the project using a fast search tool (like ripgrep)."
   :function (lambda (pattern)
               (let ((rg-cmd (format "rg --max-count 20 --no-heading --color never %s %s"
                                     (shell-quote-argument pattern)
                                     (determine-recent-project-root))))
                 (shell-command-to-string rg-cmd)))
   :args (list '(:name "pattern"
                       :type string
                       :description "Pattern to search for"))
   :confirm t
   :include nil
   :category "search")

  (gptel-make-tool
   :name "set_file_content"
   :description "Set the content of a file to the given string. Expects filename, and the full content."
   :function (lambda (filename content)
               (with-temp-file filename
                 (insert content))
               "Saved!")
   :args (list
          '(:name "filename" :type string :description "The file to overwrite.")
          '(:name "content" :type string :description "The new content for the file."))
   :confirm t
   :include t
   :category "files")

  (gptel-make-tool
   :function (lambda (url)
               (shell-command-to-string (format "w3m -dump '%s'" url)))
   :name "read_webpage"
   :description "Read the contents of a URL"
   :args (list '(:name "url"
                       :type string
                       :description "The URL to read"))
   :category "web")

  (gptel-make-tool
   :function (lambda (phrase)
               (if (string-match-p "^http" phrase)
                   (shell-command-to-string (format "w3m -dump '%s'" phrase))
                 (shell-command-to-string
                  (format
                   "w3m -dump 'https://duckduckgo.com/?q=%s'"
                   (url-hexify-string phrase)))))
   :name "search_web"
   :description "Search the web for a string."
   :args (list '(:name "phrase"
                       :type string
                       :description "The keywords to search for on the web, just the KEYWORDS"))
   :category "web")

  (advice-add 'keyboard-quit :before (lambda (&rest args) (ignore-errors (gptel-abort (current-buffer)))))

  (defun timfel/gptel-complete ()
    (interactive
     (let ((query (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end))
                    (buffer-substring-no-properties (point-min)
                                                    (point))))
           (gptel-use-tools nil)
           (gptel-include-reasoning nil))
       (gptel-request query
                      :callback (lambda (response info)
                                  (let* ((start-marker (plist-get info :position))
                                         (tracking-marker (plist-get info :tracking-marker)))
                                    (if (stringp response)
                                        (save-excursion
                                          (with-current-buffer (marker-buffer start-marker)
                                            (goto-char (or tracking-marker start-marker))
                                            (insert response)
                                            (plist-put info :tracking-marker (setq tracking-marker (point-marker))))))))
                      :stream gptel-stream
                      :system "Continue writing until the current control flow is completed or the task described in the last comment is done. Only write code, no markup, no communication, no explanations, do not repeat parts of the request, just continue writing the code."))))

  (setq gptel-directives (let* ((promptdir (expand-file-name "prompts" user-emacs-directory))
                                (prompt-files (directory-files promptdir t "md$")))
                           (mapcar (lambda (prompt-file)
                                     ;; (list (intern (f-base prompt-file)) "filler1" "filler2")
                                     (with-temp-buffer
                                       (insert-file-contents prompt-file)
                                       (let ((prompt-description "NO DESCRIPTION")
                                             (prompt-text nil))
                                         ;; nab the description - single-line descriptions only!
                                         (goto-char (point-min))
                                         (when (re-search-forward "#\\+description: \\(.*?\\) *--> *$" nil t)
                                           (setq prompt-description (match-string 1)))
                                         ;; remove all comments
                                         (delete-matching-lines "^ *<!--" (point-min) (point-max))
                                         (delete-matching-lines "^$" (point-min) (+ 1 (point-min))) ; remove first blank line if exists
                                         (goto-char (point-min)) ;; not necessary, point is in the midst of comments to start
                                         ;; return the megillah
                                         (list
                                          (intern (file-name-directory prompt-file)) ; gptel-directives key
                                          prompt-description
                                          (buffer-substring-no-properties (point-min) (point-max)) ))))
                                   prompt-files)))
  :bind (("C-x a i" . gptel-send)
         ("C-x a c" . timfel/gptel-complete)))

(use-package llm-tool-collection
  :ensure t
  :after gptel
  :vc (:url "https://github.com/skissue/llm-tool-collection" :branch "main" :rev :newest)
  :config

  (llm-tool-collection-deftool list-buffers
    (:category "buffers" :tags (buffers editing))
    nil
    "Get the list of files the user has open in buffers."
    (string-join
     (remove nil (mapcar #'buffer-file-name
                         (buffer-list)))
     "\n"))

  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-category "filesystem"))
  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-category "buffers"))
  (setq
   gptel-use-tools t
   gptel-confirm-tool-calls 'auto
   gptel-tools
        (let ((funcs nil)
              (names (list
                      "get_recently_edited_filenames"
                      "search_in_project"
                      "set_file_content"
                      "read_webpage"
                      "search_web"
                      "change_directory"
                      "get_current_directory"
                      "execute_command"
                      "view_buffer"
                      "read_file"
                      "list_directory"
                      "list_buffers"
                      "create_file"
                      "patch_file"
                      "create_directory")))
          (dolist (category gptel--known-tools)
            (dolist (pair (cdr category))
              (when (member (car pair) names)
                (push (cdr pair) funcs))))
          funcs)))

(setq gist-location
      (cl-case system-type
        (windows-nt "D:/gists")
        (t (expand-file-name "~/dev/gists/"))))

(use-package oca
  :load-path gist-location
  :after (gptel)
  :commands oca-key
  :if (file-exists-p (concat gist-location "/oca.el"))
  :demand t)

(use-package orcl
  :load-path gist-location
  :commands (timfel/git-merges-jira-html jira)
  :config
  (require 'jira)
  (defun jira ()
    ;; some jira instance fails to get some basic data, make sure fields and
    ;; statuses are there
    (interactive)
    (condition-case nil
        (jira-api-get-basic-data)
      (error nil))

    (jira-api-get-users)
    (jira-api-get-fields)
    (jira-api-get-statuses)
    (jira-api-get-resolutions)
    ;; (jira-api-get-filters :force t)
    (jira-api-get-projects)
    ;; (jira-api-get-account-id)

    (funcall-interactively #'jira-issues))
  :if (file-exists-p (concat gist-location "/orcl.el")))

(use-package impatient-mode
  :commands impatient-mode
  :ensure t)

(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  :config
  (buffer-terminator-mode 1)
  (add-to-list 'buffer-terminator-rules-alist
               `(call-function . ,(lambda ()
                                    (if (not (seq-filter (lambda (x) (string-suffix-p x (buffer-name)))
                                                         '(".java")))
                                        :keep
                                      nil))))
  (add-to-list 'buffer-terminator-rules-alist
               `(call-function . ,(lambda ()
                                    (if (> (/ buffer-terminator-interval 2)
                                           (time-convert (current-idle-time) 'integer))
                                        :keep
                                      nil)))))

(use-package xt-mouse
  :if (eq window-system nil)
  :commands xterm-mouse-mode
  :config (run-with-idle-timer 0.1 nil #'xterm-mouse-mode +1))

(use-package clipetty
  :ensure t
  :if (and (eq window-system nil)
           (eq system-type 'gnu/linux)
           (not (getenv "WAYLAND_DISPLAY")))
  :hook (after-init . global-clipetty-mode))

(use-package emacs-ci
  :commands ci-dashboard
  :load-path "~/.emacs.d/lisp/ci-dashboard"
  :if (file-exists-p "~/.emacs.d/lisp/ci-dashboard/emacs-ci.el"))

(use-package proced
  :ensure t
  :bind (("<f8>". proced)
         :map proced-mode-map
         ("<f8>" . quit-window))
  :config
  (setq proced-enable-color-flag t))

(use-package multi-vterm
  :ensure t
  :commands (multi-vterm)
  :if (not (eq system-type 'windows-nt))
  :bind (("<f12>" . (lambda ()
                      (interactive)
                      (require 'multi-vterm)
                      (select-window
                       (split-window
	                (selected-window)
	                (- (multi-vterm-current-window-height) (multi-vterm-dedicated-calc-window-height))))
                      (let ((buf (seq-find (lambda (b)
                                             (let ((n (buffer-name b)))
                                               (and n (string-prefix-p "*vterminal" n))))
                                           (buffer-list))))
                        (if buf
                            (switch-to-buffer buf)
                          (multi-vterm)
                          (add-hook 'kill-buffer-hook #'delete-window 0 t)))))
         :map vterm-mode-map
         ("C-x C-f" . (lambda ()
                        (interactive)
                        (when vterm--process
                          (let* ((pid (process-id vterm--process))
                                 (dir (file-truename (format "/proc/%d/cwd/" pid))))
                            (setq default-directory dir)))
                        (call-interactively (keymap-lookup (current-global-map) "C-x C-f"))))
         ("C-x <right>" . multi-vterm-next)
         ("C-x <left>" . multi-vterm-prev)
         ("C-x c" . multi-vterm)
         ("<f12>" . delete-window))
  :config (setq multi-vterm-dedicated-window-height-percent 40
                vterm-max-scrollback 40000))

(use-package eshell
  :if (eq system-type 'windows-nt)
  :after exec-path-from-shell
  :bind (("<f12>" . (lambda ()
                      (interactive)
                      (let ((b (get-buffer-create "*eshell*")))
                        (if-let ((w (get-window-with-predicate (lambda (w) (eq b (window-buffer w))))))
                            (delete-window w)
                          (let ((w (split-window (selected-window)
                                                 (let ((edges (window-edges)))
                                                   (round (* 0.7 (- (nth 3 edges) (nth 1 edges))))))))
                            (select-window w)
                            (set-window-buffer w b)
                            (with-current-buffer b
                              (unless (derived-mode-p 'eshell-mode) (eshell-mode))))))))))

(use-package transpose-frame
  :commands (transpose-frame flip-frame flop-frame rotate-frame rotate-frame-clockwise)
  :ensure t)

(use-package jira
  :ensure t
  :defer t
  :config
  (add-to-list 'transient-values
               '(jira-issues-menu "--myself" "--resolution=Unresolved"))
  :custom
  (jira-issues-max-results 70)
  (jira-token-is-personal-access-token t)
  (jira-users-max-results 50)
  (jira-api-version 2)
  (jira-debug nil))

;; (use-package-report)
