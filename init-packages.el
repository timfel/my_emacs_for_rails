(require 'compile)
(require 'cc-mode)


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("cselpa" . "https://elpa.thecybershadow.net/packages/"))
(if (version< emacs-version "27")
    (package-initialize))

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
;; (use-package doom-modeline
;;   :demand t
;;   :after all-the-icons
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode)
;;   :config (setq doom-modeline-minor-modes nil
;;                 doom-modeline-buffer-file-name-style 'truncate-all))

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
  :init (progn
          (setq lsp-print-io nil
                lsp-enable-snippet t
                lsp-enable-indentation nil
                lsp-before-save-edits t
                lsp-enable-file-watchers nil)

          (if (not (f-exists-p lsp-clients-emmy-lua-jar-path))
              (url-copy-file
               "https://github.com/EmmyLua/EmmyLua-LanguageServer/releases/download/0.3.6/EmmyLua-LS-all.jar"
               lsp-clients-emmy-lua-jar-path))))
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
            (setq gc-cons-threshold 100000000) ;; 100mb
            (setq lsp-prefer-capf t)
            (setq read-process-output-max (* 1024 1024)) ;; 1mb
            ;; settings
            (setq lsp-ui-flycheck-live-reporting t
                  lsp-print-performance nil
                  lsp-report-if-no-buffer t
                  lsp-enable-snippet t
                  lsp-enable-xref t
                  lsp-enable-completion-at-point t
                  lsp-response-timeout 10
                  lsp-eldoc-render-all t
                  lsp-ui-peek-always-show t
                  lsp-ui-doc-position 'top
                  lsp-ui-doc-use-webkit t
                  lsp-ui-sideline-enable nil
                  lsp-ui-sideline-show-symbol t
                  lsp-ui-sideline-show-hover t
                  lsp-ui-sideline-showcode-actions nil
                  lsp-ui-sideline-ignore-duplicate t
                  lsp-ui-sideline-delay 2
                  lsp-idle-delay 1.000
                  lsp-ui-sideline-code-actions-prefix "💡 "
                  lsp-ui-sideline-update-mode 'line)
            (if (equalp (getenv "WSL") "1")
                (setq lsp-ui-flycheck-live-reporting nil
                      lsp-response-timeout 5
                      lsp-ui-sideline-enable nil))))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package lsp-python-ms
  :ensure t
  :after (lsp-mode)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)))
  :config (setq
           lsp-python-ms-python-executable-cmd "python3")
  )

(use-package lsp-java
  :ensure t
  :after (lsp-mode flycheck company)
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
             lsp-java-save-actions-organize-imports t
             lsp-java-format-on-type-enabled nil
             lsp-java-format-comments-enabled nil
             lsp-java-format-enabled nil
             lsp-java-autobuild-enabled nil
             lsp-java-completion-import-order ["java" "javax" "org" "com"]
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
                        (set-fill-column 100)
                        (c-set-offset 'arglist-cont-nonempty 16)))
            (define-key java-mode-map (kbd "C-S-o") #'lsp-java-organize-imports)
            (add-hook 'java-mode-hook #'lsp)
            ;; (add-hook 'java-mode-hook 'doom-modeline-mode)
            (add-hook 'java-mode-hook 'friendly-whitespace)
            (add-hook 'java-mode-hook (lambda () (flycheck-mode t)))
            (add-hook 'java-mode-hook (lambda () (company-mode t)))
            ;; (add-hook 'java-mode-hook (lambda () (lsp-ui-flycheck-enable t)))
            ;; (add-hook 'java-mode-hook (lambda () (lsp-ui-sideline-mode t)))
                      ))

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

(defun my/lsp/run-mx-command (command)
  (interactive (let ((default-value (condition-case nil (car mx-run-history) (error "mx python"))))
                 (list (read-string (format "Command (%s): " default-value) nil 'mx-run-history default-value))))
  (let* ((default-directory (magit-toplevel))
         (current-win (get-buffer-window (current-buffer))))
    (call-interactively 'shell)
    (execute-kbd-macro [return])
    (execute-kbd-macro (format "cd %s" default-directory))
    (execute-kbd-macro [return])
    (sit-for 1)
    (delete-window (get-buffer-window (get-buffer "*shell*")))
    (let ((win (display-buffer-in-side-window (get-buffer "*shell*") `((side . bottom)
                                                                       (slot . -1)
                                                                       (window-height . 10)))))
      (select-window win)
      (execute-kbd-macro command)
      (execute-kbd-macro [return])
      (when (string-match-p "mx -d " command)
        (sit-for 1)
        (select-window current-win)
        (call-interactively 'dap-debug)))))

(defun my/lsp/build-mx-project (command)
  (interactive (let ((default-value (condition-case nil (car mx-build-history) (error "mx build"))))
                 (list (read-string (format "Command (%s): " default-value) nil 'mx-build-history default-value))))
  (let* ((default-directory (magit-toplevel))
         (buff "*build-mx-project*"))
    (start-process "build-mx-project" buff "bash" "-ic" command)
    (let ((win (display-buffer-in-side-window (get-buffer buff) `((side . bottom)
                                                                  (slot . 0)
                                                                  (window-height . 10)))))
      (select-window win)
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "q") 'delete-window)
        (use-local-map map))
      (call-interactively 'end-of-buffer))))

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
(use-package dap-mode
  :ensure t :after lsp-mode
  :config (progn
            ;; show/hide debug and utility windows naturally
            (setq my/repl-should-show-hydra nil)
            (defun my/repl-show-hydra ()
              (if (equalp (buffer-name (window-buffer (selected-window))) "*dap-ui-repl*")
                  (progn
                    (setq my/repl-should-show-hydra t)
                    (run-at-time 0.5 nil #'dap-hydra/nil))
                (if (and (not (active-minibuffer-window))
                         my/repl-should-show-hydra)
                    (progn
                      (setq my/repl-should-show-hydra nil)
                      (run-at-time 0.5 nil #'dap-hydra)))))
            (add-hook 'buffer-list-update-hook #'my/repl-show-hydra)

            (defun my/show-debug-windows (session)
              (save-excursion
                (call-interactively #'dap-ui-repl)
                (delete-window (get-buffer-window "*dap-ui-repl*"))
                (display-buffer-in-side-window (get-buffer "*dap-ui-repl*") `((side . bottom)
                                                                              (slot . 1)
                                                                              (window-height . 10)))
                (seq-mapn (lambda (name func)
                            (unless (-> (-compose 'buffer-name 'window-buffer)
                                        (-map (window-list))
                                        (-contains? name))
                              (call-interactively func))
                            (with-current-buffer name
                              (visual-line-mode 1)))
                          (list dap-ui--breakpoints-buffer dap-ui--expressions-buffer dap-ui--locals-buffer dap-ui--sessions-buffer)
                          '(dap-ui-breakpoints dap-ui-expressions dap-ui-locals dap-ui-sessions))))
            (add-hook 'dap-session-created-hook 'my/show-debug-windows)

            (defun my/close-debug-windows (session)
              (condition-case nil
                  (delete-window (get-buffer-window "*dap-ui-repl*" )))
              (seq-map (lambda (name)
                         (condition-case nil
                             (kill-buffer name)
                           (error nil)))
                       (list dap-ui--breakpoints-buffer dap-ui--expressions-buffer dap-ui--locals-buffer dap-ui--sessions-buffer)))
            (add-hook 'dap-terminated-hook 'my/close-debug-windows)

            (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
            (add-hook 'dap-terminated-hook (lambda (arg) (call-interactively #'dap-hydra/nil)))

            ;; default settings
            (dap-mode 1)
            (dap-ui-mode 1)
            (dap-tooltip-mode 1)
            ;; (require 'dap-gdb-lldb)

            (dap-register-debug-template
             "GDB Stratagus-Dbg + Wargus"
             (list :type "gdb"
                   :request "launch"
                   :name "GDB Stratagus-Dbg + Wargus"
                   :target "/home/tim/Dev/stratagus/dev/stratagus/build/stratagus-dbg"
                   :arguments "-d /home/tim/.stratagus/data.Wargus -W"
                   :cwd "/home/tim/Dev/stratagus/dev/wargus"))

            (dap-register-debug-template
             "GDB Stratagus-Dbg + War1gus"
             (list :type "gdb"
                   :request "launch"
                   :name "GDB Stratagus-Dbg + War1gus"
                   :target "/home/tim/Dev/stratagus/dev/stratagus/build/stratagus-dbg"
                   :arguments "-d /home/tim/.stratagus/data.War1gus -W"
                   :cwd "/home/tim/Dev/stratagus/dev/war1gus"))
            
            (tooltip-mode 1)
            (define-key dap-ui-session-mode-map [C-mouse-1] 'dap-ui-session-select)
            (setq dap-auto-show-output nil)))

(use-package dap-lldb
  :after dap-mode
  :config (progn
            (setq
             dap-lldb-debug-program
             `(,(expand-file-name "~/.emacs.d/llvm-project/lldb/build/bin/lldb-vscode")))

            (defun dap-lldb-attach (file)
              (interactive "f")
              (let ((pid (shell-command-to-string (format "pidof %s" (f-base file)))))
                (dap-debug (list :type "lldb"
                                 :request "attach"
                                 :program file
                                 :pid pid
                                 :name "LLDB::Attach"))))))

(use-package dap-hydra
  :after dap-mode)

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

(if (version<= "26" emacs-version)
    (use-package posframe :ensure t))

(use-package dap-java
  :after (dap-mode lsp-java)
  :config (progn
            (setq dap-java-default-debug-port 8000)

            ;; bind C-c C-d dynamically
            (fset 'my/dap-debug 'dap-debug)
            (add-hook 'dap-session-created-hook
                      (lambda (arg) (fset 'my/dap-debug 'dap-hydra)))
            (add-hook 'dap-terminated-hook
                      (lambda (arg) (fset 'my/dap-debug 'dap-debug)))

            (define-key java-mode-map (kbd "C-c C-d") #'my/dap-debug)
            (define-key java-mode-map (kbd "C-c C-c") #'my/lsp/build-mx-project)
            (define-key java-mode-map (kbd "C-c m") #'my/lsp/run-mx-command)
            (define-key java-mode-map (kbd "C-c C-x t") #'dap-breakpoint-toggle)

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

(use-package wl
  :ensure wanderlust
  :commands (wl)
  :bind (:map wl-summary-mode-map
              ;;Swap a and A in summary mode, so citing original message is on a and no-cite on A.
              ("A" . wl-summary-reply)
              ("a" . wl-summary-reply-with-citation))
  :hook ((wl-mail-send-pre . djcb-wl-draft-subject-check)
         (wl-mail-send-pre . djcb-wl-draft-attachment-check))
  :config (progn
            (print "Wanderlust configured")
            
            ;; Check messages for missing subject or abstract
            (defun djcb-wl-draft-subject-check ()
              "check whether the message has a subject before sending"
              (if (and (< (length (std11-field-body "Subject")) 1)
                       (null (y-or-n-p "No subject! Send current draft?")))
                  (error "Abort.")))

            (defun djcb-wl-draft-attachment-check ()
              "if attachment is mention but none included, warn the the user"
              (save-excursion
                (goto-char 0)
                (unless ;; don't we have an attachment?
                    (re-search-forward "^Content-Disposition: attachment" nil t)
                  (when ;; no attachment; did we mention an attachment?
                      (or (re-search-forward "attach" nil t)
                          (re-search-forward "anhang" nil t)
                          (re-search-forward "anbei" nil t)
                          (re-search-forward "adjunt" nil t))
                    (unless (y-or-n-p "Possibly missing an attachment. Send current draft?")
                      (error "Abort."))))))
            
            ;; tfel: I have a bug somewhere when creating drafts where a number is
            ;; passed to elmo-date-get-datevec, which needs a timestamp string. we
            ;; fall back to the current time in that case.
            (define-advice wl-summary-reply (:around (orig-fun &rest args) set-msg-id)
              (setq current-msg-number (random 100))
              (apply orig-fun args))

            (define-advice elmo-date-get-datevec (:around (orig-fun &rest args) convert-elmo-datevec-arg)
              (if (stringp (car args))
                  (apply orig-fun args)
                (timezone-fix-time (current-time-string) (current-time-zone) nil)))

            (define-advice wl-draft-config-info-filename (:around (orig-fun &rest args) ensure-draft-filename)
              (if (not (numberp (car args)))
                  (setcar args current-msg-number))
              (apply orig-fun args))

            (define-advice wl-draft-config-info-operation (:around (orig-fun &rest args) ensure-draft-filename)
              (if (not (numberp (car args)))
                  (setcar args current-msg-number))
              (apply orig-fun args))

            (if (boundp 'mail-user-agent)
                (setq mail-user-agent 'wl-user-agent))
            (if (fboundp 'define-mail-user-agent)
                (define-mail-user-agent
                  'wl-user-agent
                  'wl-user-agent-compose
                  'wl-draft-send
                  'wl-draft-kill
                  'mail-send-hook)))
  :init (setq
         wl-init-file (expand-file-name "~/.emacs.d/wanderlust/wl.el")
         wl-address-file (expand-file-name "~/.emacs.d/wanderlust/addresses")
         wl-folders-file (expand-file-name "~/.emacs.d/wanderlust/folders")
         ;; SMTP server for mail posting.
         wl-smtp-posting-server "stbeehive.oracle.com"
         wl-smtp-posting-port 465
         wl-smtp-posting-user "tim.felgentreff@oracle.com"
         wl-smtp-authenticate-type "login"
         wl-smtp-connection-type 'ssl
         wl-from "tim.felgentreff@oracle.com"
         smtp-local-domain "localhost"

         ;; Do not cache passwords. The cache corrupts server
         ;; secrets.
         password-cache nil

         elmo-imap4-default-user "tim.felgentreff@oracle.com"
         elmo-imap4-default-server "stbeehive.oracle.com"
         elmo-imap4-default-port 993
         elmo-imap4-default-authenticate-type 'clear
         elmo-imap4-default-stream-type 'ssl
         elmo-passwd-storage-type 'auth-source

         ;; Location of archives
         elmo-archive-folder-path "~/Mail"

         ;; Location of MH and Maildir folders
         elmo-localdir-folder-path "~/Mail/"
         elmo-maildir-folder-path "~/Mail/"

         wl-message-id-domain "tim.felgentreff@oracle.com"
         wl-from "Tim Felgentreff <tim.felgentreff@oracle.com>"
         ;; mime-edit-default-signature "~/OnlineFolder/Library/dot.signature"
         wl-forward-subject-prefix "Fwd: "

         wl-default-folder "%Inbox" ;; my main inbox
         ;; wl-biff-check-folder-list '("%Inbox") ;; check every 180 seconds
         ;; wl-biff-check-interval 180

         wl-draft-folder "%Drafts"  ;; store drafts in 'postponed'
         wl-trash-folder "%Trash"   ;; put trash in 'trash'

         wl-stay-folder-window t
         wl-folder-window-width 25
         wl-folder-use-frame nil

         wl-summary-always-sticky-folder-list t
         wl-summary-line-format "%n%T%P %D/%M (%W) %h:%m %t%[%25(%c %f%) %] %s"
         wl-summary-width nil

         wl-message-ignored-field-list '("^.*")
         wl-message-visible-field-list '("^From:" "^To:" "^Cc:" "^Date:" "^Subject:")
         wl-message-sort-field-list wl-message-visible-field-list
         wl-summary-default-sort-spec 'date
         wl-message-window-size '(1 . 2)

         ;; Always download emails without confirmation
         wl-prefetch-threshold nil
         wl-message-buffer-prefetch-threshold nil
         elmo-message-fetch-threshold nil
         elmo-folder-update-threshold nil
         elmo-network-session-idle-timeout 120

         ;; Rendering of messages using 'shr', Emacs' simple html
         ;; renderer, but without fancy coloring that distorts the
         ;; looks
         mime-view-text/html-previewer 'shr
         ;; shr-use-colors nil

         ;; wl-draft-config-alist
         ;; '(((string-match "1" "1")
         ;;    (bottom . "\n--\n") (bottom-file . "~/OnlineFolder/Library/dot.signature"))
         ;;   )

         mime-edit-split-message nil
         )
  )

(use-package bbdb
  :ensure t
  :after (wl)
  :commands (bbdb-initialize)
  :hook
  ((mail-setup . bbdb-mail-aliases)
   (message-setup . bbdb-mail-aliases)
   (wl-mail-setup . jjgr-add-bbdb-tab-completion))

  :init
  (setq bbdb-file "~/.emacs.d/bbdb"
        bbdb-mua-pop-up t
        bbdb-mua-pop-up-window-size t)

  :config
  (progn
    (bbdb-initialize 'wl)
    (bbdb-mua-auto-update-init 'wl)

    (setq
     bbdb-offer-save 1                        ;; 1 means save-without-asking
     
     bbdb-use-pop-up t                        ;; allow popups for addresses
     bbdb-electric-p t                        ;; be disposable with SPC
     bbdb-popup-target-lines  1               ;; very small
     
     bbdb-dwim-net-address-allow-redundancy t ;; always use full name
     bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

     bbdb-always-add-address t                ;; add new addresses to existing...
     ;; ...contacts automatically
     bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

     bbdb-completion-type nil                 ;; complete on anything

     bbdb-complete-name-allow-cycling t       ;; cycle through matches
     ;; this only works partially

     bbbd-message-caching-enabled t           ;; be fast
     bbdb-use-alternate-names t               ;; use AKA


     bbdb-elided-display t                    ;; single-line addresses

     ;; auto-create addresses from mail
     bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook   
     bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
     ;; NOTE: there can be only one entry per header (such as To, From)
     ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

     '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))

    (defun my-bbdb-complete-mail ()
      "If on a header field, calls `bbdb-complete-mail' to complete the name."
      (interactive)
      (when (< (point)
               (save-excursion
                 (goto-char (point-min))
                 (search-forward (concat "\n" mail-header-separator "\n") nil 0)
                 (point)))
        (bbdb-complete-mail)))

    (defun jjgr-add-bbdb-tab-completion ()
      (define-key (current-local-map) (kbd "<tab>")
        'my-bbdb-complete-mail))
    )
  )

(use-package calfw
  :ensure t
  :init (progn
          (require 'calfw-org)
          (setq cfw:org-overwrite-default-keybinding t)))
(use-package org-caldav
  :ensure t
  :config (progn
            (let ((oracle-cal (expand-file-name "~/org/oracle-cal.org"))
                  (graalvm-cal (expand-file-name "~/org/graalvm-cal.org")))
              (setq
               org-caldav-calendars
               `((:url "https://stbeehive.oracle.com/caldav/st/home/tim.felgentreff%40oracle.com/calendars/"
                       :calendar-id "MyCalendar"
                       :inbox ,oracle-cal)
                 (:url "https://stbeehive.oracle.com/caldav/st/home/GRAALVM-SHARED-CALENDAR_WW%40oracle.com/calendars/"
                       :calendar-id "MyCalendar"
                       :inbox ,graalvm-cal))
               ;; org-caldav-url "https://stbeehive.oracle.com/caldav/st/home/tim.felgentreff%40oracle.com/calendars/"
               ;; org-caldav-calendar-id "MyCalendar"
               ;; org-caldav-inbox (expand-file-name "~/.emacs.d/caldav-calendar.org")
               org-caldav-sync-direction 'cal->org ;; never push org to calendar, i use this readonly
               org-icalendar-timezone "UTC"
               org-caldav-delete-org-entries t
               org-caldav-delete-calendar-entries nil)
              (add-to-list 'org-agenda-files oracle-cal)
              (add-to-list 'org-agenda-files graalvm-cal)
              org-agenda-files
            )))

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

(autoload 'kickasm-mode "kickasm-mode" "KickAssembler mode" t)
(add-hook 'kickasm-mode-hook
          (lambda () (add-hook 'before-save-hook
                               (lambda ()
                                 (whitespace-cleanup)
                                 (indent-region (point-min) (point-max) nil)
                                 (untabify (point-min) (point-max)))
                               nil
                               'local)))
