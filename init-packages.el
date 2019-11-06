(require 'compile)
(require 'cc-mode)


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("cselpa" . "https://elpa.thecybershadow.net/packages/"))
(package-initialize)

(condition-case nil
    (require 'use-package)
  (file-error
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

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
                                                   (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
                                                   (define-key ruby-mode-map "\M-\C-o" 'rct-complete-symbol))))
            (add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
            (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))))
(use-package org
  :ensure t
  :config (progn
            ;; (add-hook 'org-mode-hook (lambda ()
            ;;                            (progn
            ;;                              (local-set-key (kbd "<return>") 'org-insert-heading)
            ;;                              (local-set-key (kbd "M-<return>") 'org-insert-subheading)))
            (add-hook 'org-mode-hook (lambda () (run-at-time "1 sec" nil (lambda () (fci-mode 0)))))
            (global-set-key (kbd "C-c a") 'org-agenda)
            (define-key global-map (kbd "C-c c") 'org-capture)
            (let ((todos (expand-file-name "~/OneDrive/todo.org"))
                  (notes (expand-file-name "~/OneDrive/notes.org")))
              (setq
               org-default-notes-file notes
               org-agenda-files (list todos)
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
               org-capture-templates
               (list
                ;; schedule new todo items to today by default
                (list "n" "note" 'entry (list 'file+datetree notes) "* %?\nEntered on %U\n  %i\n  %a")
                (list "t" "todo" 'entry (list 'file+headline todos "Tasks") "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"))))))

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
(use-package yasnippet
  :ensure t
  :config (yas-global-mode t))
(use-package company
  :ensure t
  :config (progn
            (global-company-mode t)
            (global-set-key (kbd "M-?") 'company-complete)))
;; (use-package company-box
;;  :ensure t
;;  :hook (company-mode . company-box-mode))
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


;; Git(hub)
;; (use-package gist :ensure t)
;; (use-package magit-popup :ensure t)
(use-package magit
  :ensure t
  :config (progn
            (global-set-key (kbd "C-x C-z") 'magit-status)
            ;; (add-hook 'magit-mode-hook 'magit-load-config-extensions)
            ;; (setq with-editor-emacsclient-executable "/usr/bin/emacsclient-snapshot")
            (setq magit-auto-revert-tracked-only t)
            (magit-auto-revert-mode)))
;; (use-package magit-svn :ensure t)
;; (use-package gh :ensure t)


;; Tools
(use-package ace-window
  :ensure t
  :config (progn
            (global-set-key (kbd "C-x o") 'ace-window)))
(use-package fic-mode :ensure t)
(use-package request :ensure t)
(use-package mw-thesaurus
  :ensure t
  :after request)
(use-package cssh :ensure t)
(use-package switch-window :ensure t)
(use-package autopair :ensure t)
(use-package popup :ensure t)
(use-package fuzzy :ensure t)
(use-package pcache :ensure t)
(use-package logito :ensure t)
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

(use-package all-the-icons
  :demand t
  :ensure t)
(use-package doom-modeline
  :demand t
  :after all-the-icons
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config (setq doom-modeline-minor-modes nil
                doom-modeline-buffer-file-name-style 'truncate-all))

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
  :ensure t
  :init (setq lsp-print-io nil
              lsp-enable-snippet t
              lsp-enable-indentation nil
              lsp-before-save-edits t
              lsp-enable-file-watchers nil))
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
  :commands company-lsp
  :after company
  :init (add-to-list 'company-backends #'company-lsp)
  :config (setq company-lsp-cache-candidates t
                company-lsp-filter-candidates t))
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :config (progn
            (define-key lsp-mode-map (kbd "C-.") #'helm-imenu)
            ;; (define-key lsp-mode-map (kbd "C-S-t") #'lsp-ui-find-workspace-symbol)
            (define-key lsp-mode-map (kbd "C-S-t") #'helm-lsp-workspace-symbol)
            (define-key lsp-mode-map (kbd "C-M-,") (lambda ()
                                                     (interactive)
                                                     (list-flycheck-errors)
                                                     (switch-to-buffer-other-window flycheck-error-list-buffer)))
            (define-key lsp-mode-map (kbd "C-,") #'lsp-execute-code-action)
            ;; (lsp-ui-peek-jump-backward)
            ;; (lsp-ui-peek-jump-forward)
            (define-key lsp-ui-mode-map (kbd "M-,") #'lsp-treemacs-errors-list)
            (define-key lsp-ui-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
            (define-key lsp-ui-mode-map (kbd "C-M-.") #'lsp-ui-peek-find-references)
            ;; (define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
            ;; (define-key lsp-mode-map (kbd "C-M-.") #'lsp-find-references)
            (setq lsp-ui-flycheck-live-reporting t
                  lsp-print-performance nil
                  lsp-report-if-no-buffer t
                  lsp-enable-snippet t
                  lsp-enable-xref t
                  lsp-enable-completion-at-point t
                  lsp-response-timeout 2
                  lsp-ui-doc-use-webkit t
                  lsp-ui-sideline-enable t
                  lsp-ui-sideline-show-symbol t
                  lsp-ui-sideline-show-hover t
                  lsp-ui-sideline-showcode-actions t
                  lsp-ui-sideline-ignore-duplicate t
                  lsp-ui-sideline-delay 2
                  lsp-ui-sideline-code-actions-prefix "💡 "
                  lsp-ui-sideline-update-mode 'line)))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package lsp-java
  :ensure t
  :after (lsp-mode flycheck company)
  :config (progn
            (require 'lsp-ui-flycheck)
            (require 'lsp-ui-sideline)
            ;; (setq lsp-java-workspace-dir "/home/tim/eclipse-workspace/")
            (setq
             lsp-java-save-actions-organize-imports t
             lsp-java-format-on-type-enabled nil
             lsp-java-format-comments-enabled nil
             lsp-java-format-enabled nil
             lsp-java-autobuild-enabled nil
             lsp-java-import-order ["java" "javax" "org" "com"])

            ;; (puthash "language/progressReport" (lambda (workspace params)
            ;;                                      (lsp-java--progress-report workspace params)
            ;;                                      (-let [(&hash "status" "complete") params]
            ;;                                        (when complete
            ;;                                          (message "Build complete, running mx")
            ;;                                          (let ((default-directory (file-name-directory (buffer-file-name (current-buffer)))))
            ;;                                            (start-process
            ;;                                             "mx-nativebuild"
            ;;                                             "*mx output*"
            ;;                                             "~/.graalenv/mx/mx"
            ;;                                             "nativebuild")))))
            ;;          (lsp--client-notification-handlers (gethash 'jdtls lsp-clients)))
            ;; adjust open list indentation
            (add-hook 'java-mode-hook
                      (lambda ()
                        (c-set-offset 'arglist-cont-nonempty 16)))
            (add-hook 'java-mode-hook #'lsp)
            (add-hook 'java-mode-hook 'doom-modeline-mode)
            (add-hook 'java-mode-hook 'friendly-whitespace)
            (add-hook 'java-mode-hook (lambda () (flycheck-mode t)))
            (add-hook 'java-mode-hook (lambda () (company-mode t)))
            (add-hook 'java-mode-hook (lambda () (lsp-ui-flycheck-enable t)))
            (add-hook 'java-mode-hook (lambda () (lsp-ui-sideline-mode t)))))

(defun my/lsp/find-eclipse-projects-recursively (directory)
  (let ((current-directory-list (directory-files directory)))
    (seq-concatenate 'list
     (if (seq-some (lambda (elt) (string-equal ".project" elt)) current-directory-list)
         (list directory)
       '())
      (seq-mapcat (lambda (elt) (my/lsp/find-eclipse-projects-recursively (concat (file-name-as-directory directory) elt)))
                  (seq-filter (lambda (elt) (and (file-directory-p (concat (file-name-as-directory directory) elt))
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

(defun my/lsp/kill-all-java-buffers ()
  (interactive)
  (let ((list (buffer-list)))
    (dolist (buffer list)
      (let ((name (buffer-name buffer))
            (mode (with-current-buffer buffer major-mode)))
        (if (eq mode 'java-mode)
            (if (not (buffer-modified-p buffer))
                (kill-buffer buffer)))))))

(defun my/lsp/import-all-eclipse-projects ()
  (interactive)
  (let* ((base-dir (read-directory-name "Base directory to search projects in: "))
         (base-dirs (completing-read-multiple "Base sub-directories to search projects in: " (directory-files base-dir) nil t))
         (projects-found (seq-mapcat (lambda (elt) (my/lsp/find-eclipse-projects-recursively (concat (file-name-as-directory base-dir) elt))) base-dirs)))
    ;; add projects to session
    (seq-do (lambda (elt)
              (let ((exp (expand-file-name elt)))
                (if (not (seq-contains (lsp-session-folders (lsp-session)) exp))
                    (progn
                      (lsp-workspace-folders-add exp)
                      (puthash 'jdtls
                               (append (gethash 'jdtls
                                        (lsp-session-server-id->folders (lsp-session)))
                                       (list exp))
                               (lsp-session-server-id->folders (lsp-session)))))))
            projects-found)
    (lsp--persist-session (lsp-session))
    (seq-do (lambda (elt) (message (format "Imported '%s'" elt))) projects-found)))

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
         (projects-to-import (completing-read-multiple "Select projects to import (comma-sep): " projects-found nil t))
         (additional-required-projects '())
         (go-again t))
    (require 'xml)
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
                  (if (seq-contains additional-required-projects name)
                      (add-to-list 'projects-to-import elt))))
              projects-found)
      ;; if we added projects to the list of projects to import, go deeper
      (setq go-again (> (length projects-to-import) go-again)))

    ;; add projects to session
    (dolist (elt projects-to-import)
      (let ((exp (expand-file-name elt)))
        (if (not (seq-contains (lsp-session-folders (lsp-session)) exp))
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
    (,(kbd "C-x C-e") . (lambda () (if mark-active (dap-eval-region) (dap-eval-thing-at-point))))
   )
  )

(defun my/dap-set-exception-breakpoint ()
  (interactive)
  (dap--send-message (dap--make-request
                      "setExceptionBreakpoints"
                      (list :filters (list "caught" "uncaught")))
                            ;; :exceptionOptions (list :path (list :names (list "java.lang.AssertionError"))
                            ;;                         :breakMode "always")))
                     (dap--resp-handler)
                     (dap--cur-active-session-or-die)))

(defun my/dap-hotcodereplace ()
  (interactive)
  (dap--send-message (dap--make-request
                      "redefineClasses")
                     (dap--resp-handler)
                     (dap--cur-active-session-or-die)))

(define-globalized-minor-mode global-my/dap-mode my/dap-mode
  (lambda () (my/dap-mode 1)))

(use-package dap-mode
  :ensure t :after lsp-mode
  :config (progn
            (dap-mode t)
            (dap-ui-mode t)
            (setq dap-auto-show-output nil)))

(use-package dap-java
  :after (dap-mode lsp-java)
  :config (progn
            (setq dap-java-default-debug-port 8000)

            (defun my/lsp/dap-debug ()
              (interactive)
              (let ((new-frame (make-frame)))
                (select-frame-set-input-focus new-frame)
                (toggle-fullscreen)
                (call-interactively 'dap-debug)))
            (define-key java-mode-map (kbd "C-c C-d") 'my/lsp/dap-debug)
            (define-key java-mode-map (kbd "C-c C-x t") 'dap-breakpoint-toggle)

            (defun my/window-visible (b-name)
              "Return whether B-NAME is visible."
              (-> (-compose 'buffer-name 'window-buffer)
                  (-map (window-list))
                  (-contains? b-name)))

            (defun my/show-debug-windows (session)
              "Show debug windows."
              (global-my/dap-mode 1)
              (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
                (save-excursion
                  ;; display locals
                  (unless (my/window-visible dap-ui--locals-buffer)
                    (dap-ui-locals))
                  ;; display sessions
                  (unless (my/window-visible dap-ui--sessions-buffer)
                    (dap-ui-sessions)))))
            (add-hook 'dap-session-created-hook 'my/show-debug-windows)

            (defun my/close-debug-windows (session)
              (global-my/dap-mode -1)
              (let ((debug-frame (seq-find (lambda (frame)
                                             (> (seq-count (lambda (w) (string-equal (buffer-name (window-buffer w))
                                                                                     dap-ui--sessions-buffer))
                                                           (window-list frame))
                                                0))
                                           (frame-list))))
                (if debug-frame
                    (delete-frame debug-frame t))))
            (add-hook 'dap-terminated-hook 'my/close-debug-windows)

            (dap-register-debug-template "Java Attach com.oracle.graal.python"
                                         (list :type "java"
                                               :request "attach"
                                               :hostName "localhost"
                                               :projectName "com.oracle.graal.python"
                                               :port 8000))))

;; The spacemacs default colors
(condition-case nil
    (load-theme 'spacemacs-light t)
  (error
   (package-install 'spacemacs-theme)
   (load-theme 'spacemacs-light t)))


;; Flyspell options
(use-package ispell :ensure t)
(use-package flyspell :ensure t)
(add-to-list 'ispell-dictionary-alist
             '("de"
               "[a-zA-Z\304\326\334\344\366\337\374]"
               "[^a-zA-Z\304\326\334\344\366\337\374]"
               "[']" t ("-C" "-d" "de_DE") "~latin1" iso-8859-15))
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=fast"))
(setq flyspell-issue-message-flag nil)
(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "de") "english" "de")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))
(global-set-key (kbd "<f8>") 'fd-switch-dictionary)


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


;; Interactively Do Things (highly recommended, but not strictly required)
(use-package ido
  :ensure t
  :config (progn
            (ido-mode t)
            (defun ido-goto-symbol ()
              "Will update the imenu index and then use ido to select a symbol to navigate to."
              (interactive)
              (imenu--make-index-alist)
              (let ((name-and-pos '())
                    (symbol-names '()))
                (cl-flet ((addsymbols (symbol-list)
                                   (when (listp symbol-list)
                                     (dolist (symbol symbol-list)
                                       (let ((name nil) (position nil))
                                         (cond
                                          ((and (listp symbol) (imenu--subalist-p symbol))
                                           (addsymbols symbol))

                                          ((listp symbol)
                                           (setq name (car symbol))
                                           (setq position (if (overlayp (cdr symbol))
                                                              (overlay-start (cdr symbol))
                                                            (cdr symbol))))

                                          ((stringp symbol)
                                           (setq name symbol)
                                           (setq position (get-text-property 1 'org-imenu-marker symbol))))

                                         (unless (or (null position) (null name))
                                           (add-to-list 'symbol-names name)
                                           (add-to-list 'name-and-pos (cons name position))))))))
                  (addsymbols imenu--index-alist))
                (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
                       (position (cdr (assoc selected-symbol name-and-pos))))
                  (goto-char position))))
            (global-set-key [(control .)] 'ido-goto-symbol)))


;; local lisp code
(add-to-list 'load-path (locate-user-emacs-file "lisp/rml"))
(autoload 'rml-mode "rml-mode" "RML Mode" t)
(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(autoload 'darkroom-mode "darkroom-mode" "Darkroom Mode" t)
(require 'redo+)
(progn (global-set-key [(control -)] 'redo))
(require 'sudo-save)
(autoload 'pypytrace-mode "pypytrace-mode" "PyPy JIT Trace mode" t)
