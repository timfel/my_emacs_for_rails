; (condition-case nil
;     (when
; 	(load
; 	 (expand-file-name "~/.emacs.d/elpa/package.el"))
;       (package-initialize))
;   (error (let ((buffer (url-retrieve-synchronously
; 			"http://tromey.com/elpa/package-install.el")))
; 	   (save-excursion
; 	     (set-buffer buffer)
; 	     (goto-char (point-min))
; 	     (re-search-forward "^$" nil 'move)
; 	     (eval-region (point) (point-max))
; 	     (kill-buffer (current-buffer))
; 	     (when
; 		 (load
; 		  (expand-file-name "~/.emacs.d/elpa/package.el"))
; 	       (package-initialize))))))
; (setq package-archives '("tromey" . "http://tromey.com/elpa/"))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp)))))

(require 'el-get-elpa)
;; (el-get-elpa-build-local-recipes)

;; ;; CEDET hack
;; (defun inversion-test (p v)
;;   (string= v (symbol-value
;; 	      (intern-soft (concat (symbol-name p) "-version")))))
(require 'compile) ;; Needed for some reason or other. el-get fails for me, otherwise

(setq el-get-sources
      '((:name yasnippet
	       :after (progn
			 ;; (yas-load-directory (expand-file-name "../snippets" el-get-dir))
			 (yas-global-mode t)))

	(:name redo+
	       :type http
	       :url "http://www.emacswiki.org/emacs/download/redo%2b.el"
	       :load "redo_2b.el"
	       :after (progn (global-set-key [(control -)] 'redo)))

	(:name textlint
	       :type git
	       :url "git://github.com/DamienCassou/textlint.git"
	       :load "textlint.el")

	(:name color-theme-solarized
	       :depends color-theme
	       :type git
	       :url "https://github.com/sellout/emacs-color-theme-solarized.git"
	       :load "color-theme-solarized.el"
	       :build ("git checkout old-and-busted")
	       :after (progn
			(if window-system
			    (progn
			      (color-theme-solarized-light)
			      ;; Re-initialize colors when creating a new frame, to fix color-palette incompats between terminal and X
			      (defun setup-window-system-frame-colours (&rest frame)
				(color-theme-solarized-light))
			      (defadvice server-create-window-system-frame
				  (after set-window-system-frame-colours ())
				"Set custom frame colours when creating the first frame on a display"
				(message "Running after frame-initialize")
				(setup-window-system-frame-colours))
			      (ad-activate 'server-create-window-system-frame)
			      (add-hook 'after-make-frame-functions 'setup-window-system-frame-colours t)))))

	(:name coffee-mode
	       :after (progn (add-hook 'coffee-mode-hook
					   '(lambda() (progn
							;; Enable compile-on-save if there is already a *.coffee & *.js file
							(if (and (file-exists-p (buffer-file-name))
								 (file-exists-p (coffee-compiled-file-name)))
							    (coffee-cos-mode t))
							(setq coffee-args-compile '("-c" "--bare"))
							(set (make-local-variable 'tab-width) 2)
							(define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))))))

	(:name yaml-mode
	       :after (progn
				   (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
				   (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
				   (add-to-list 'auto-mode-alist '("Gemfile.lock$" . yaml-mode))))

	(:name maxframe
	       :features maxframe
	       :after (progn (add-hook 'window-setup-hook 'maximize-frame t)))

	(:name ri-emacs
	       :after (progn (setq ri-ruby-script (expand-file-name (concat el-get-dir "/ri-emacs/ri-emacs.rb")))))

	(:name rhtml-mode
	       :features rhtml-mode
	       :after (progn
			(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))
			(add-to-list 'auto-mode-alist '("\\.html.rb$" . rhtml-mode))
			(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
			))

	(:name ruby-mode
	       :after (progn
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

	(:name cucumber
	       :type git
	       :url "git://github.com/michaelklishin/cucumber.el.git"
	       :load-path "."
	       :features feature-mode
	       :depends yasnippet
	       :after (progn
				   ;; load bundle snippets
				   (yas/load-directory (expand-file-name (concat el-get-dir "/cucumber/snippets")))
				   (add-to-list 'auto-mode-alist '("\\.feature" . feature-mode))))

	(:name js2-mode
	       :after (progn
			(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
			(add-hook 'js2-mode-hook 'friendly-whitespace)
			(add-hook 'js2-mode-hook
				  (lambda () (progn
					       (setq imenu-create-index-function 'javascript-imenu-create-index)
					       (local-set-key (kbd "<return>") 'newline-and-indent)
					       (setq javascript-indent-level 2)))
				  t)))

	(:name textmate
	       :type git
	       :url "git://github.com/defunkt/textmate.el.git"
	       :load "textmate.el"
	       :after (progn
			(textmate-mode 1)
			;; Commenting blocks
			(global-set-key [(control /)] 'comment-or-uncomment-region-or-line)))

	(:name magit
	       :after (progn
			(global-set-key (kbd "C-x C-z") 'magit-status)
                        (add-hook 'magit-mode-hook 'magit-load-config-extensions)
                        ;; (setq with-editor-emacsclient-executable "/usr/bin/emacsclient-snapshot")
			(setq magit-auto-revert-mode nil)))

	(:name project-mode
	       :type git
	       :url "http://github.com/timfel/emacs-project-mode.git"
	       :load-path "."
	       :features project-mode
	       :after (progn
				   (project-mode 1)
				   (project-mode-menu)
				   (project-load-all)
				   (global-set-key "\C-t" 'project-fuzzy-search)))

	(:name org-mode
	       :after (progn
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

	(:name helm
	       :description "Emacs incremental and narrowing framework"
	       :type git
	       :build ("make")
	       :url "https://github.com/emacs-helm/helm"
	       :features helm-config)

	(:name helm-etags-plus
	       :after (progn
			(global-set-key (kbd "M-.") 'helm-etags-plus-select)))

	(:name fill-column-indicator
	       :after (progn
				   (setq fci-rule-column 81)
				   ;; (setq fci-always-use-textual-rule t)
				   ;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
				   ;; (global-fci-mode 1)
				   ))

        (:name org-journal
               :type github
               :pkgname "bastibe/org-journal"
               :before (setq org-journal-dir "~/OneDrive/Documents/journal/"))

        ;; (:name auto-complete
	;;        :after (progn
        ;;                 (require 'auto-complete-config)
        ;;                 (ac-config-default)
        ;;                 (global-auto-complete-mode t)     ;; enable global-mode
        ;;                 (setq ac-auto-start nil)            ;; automatically start
        ;;                 (setq ac-dwim 3)                  ;; Do what i mean
        ;;                 (setq ac-override-local-map nil)  ;; don't override local map
        ;;                 (ac-flyspell-workaround)
        ;;                 (setq ac-delay 5)
        ;;                 (setq ac-auto-show-menu 1)
        ;;                 (set-default 'ac-sources '(ac-source-yasnippet ac-source-semantic ac-source-abbrev ac-source-words-in-buffer))

        ;;                 (setq ac-modes (append ac-modes '(eshell-mode)))
        ;;                 (global-set-key (kbd "M-?") 'auto-complete)
        ;;                 (add-hook 'emacs-lisp-mode-hook
        ;;                           (lambda ()
        ;;                             (setq ac-sources '(ac-source-yasnippet
        ;;                                                ac-source-abbrev
        ;;                                                ac-source-words-in-buffer
        ;;                                                ac-source-symbols))))
        ;;                 (add-hook 'eshell-mode-hook
        ;;                           (lambda ()
        ;;                             (setq ac-sources '(ac-source-yasnippet
        ;;                                                ac-source-abbrev
        ;;                                                ac-source-files-in-current-dir
        ;;                                                ac-source-words-in-buffer))))))

        (:name company-mode
               :after (progn
                        (global-company-mode t)
                        (global-set-key (kbd "M-?") 'company-complete)))

        ;; (:name meghanada
        ;;        :type github
        ;;        :pkgname "mopemope/meghanada-emacs"
        ;;        :description "A Better Java Development Environment for Emacs"
        ;;        :minimum-emacs-version "24"
        ;;        :depends (yasnippet company-mode flycheck cl-lib)
        ;;        :compile "."
        ;;        :after (progn
        ;;                 (setq c-basic-offset 4)
        ;;                 ;; (add-hook 'java-mode-hook
        ;;                 ;;           (lambda ()
        ;;                 ;;             ;; meghanada-mode on
        ;;                 ;;             (meghanada-mode t)
        ;;                 ;;             (flycheck-mode +1)
        ;;                 ;;             (setq c-basic-offset 4)
        ;;                 ;;             ;; use code format
        ;;                 ;;             ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
        ;;                 ;; ))
        ;;                 )
        ;;        )

        ;; (:name emacs-eclim
        ;;        :features company-emacs-eclim
        ;;        :depends company-mode
        ;;        :after (progn
        ;;                 (global-eclim-mode)
        ;;                 (setq eclim-eclipse-dirs '("/opt/eclipse-ide-java/"))
        ;;                 (setq eclim-executable "/home/tim/.eclipse/org.eclipse.platform_4.7.2_479262390_linux_gtk_x86_64/plugins/org.eclim_2.7.2/bin/eclim")
        ;;                 (setq eclimd-autostart t)

        ;;                 ;; Displaying compilation error messages in the echo area
        ;;                 (setq help-at-pt-display-when-idle t)
        ;;                 (setq help-at-pt-timer-delay 0.1)
        ;;                 (help-at-pt-set-timer)

        ;;                 ;; Configuring company-mode
        ;;                 (company-emacs-eclim-setup)
        ;;                 (setq company-emacs-eclim-ignore-case t)

        ;;                 (defun my-java-mode-hook ()
        ;;                   (eclim-mode t))
        ;;                 (add-hook 'java-mode-hook 'my-java-mode-hook)))

	(:name emacsmirror-rcp
	       :type git
	       :url "https://github.com/edenc/emacsmirror-rcp")))

;; Auctex depends on pdflatex being available, only install if desired on this system
(if (executable-find "pdflatex")
    (setq el-get-sources
	  (append '((:name auctex
			   :after (progn
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
				    (setq TeX-parse-self t)

                                    ;; Latex
                                    ;; (require 'dbus)
                                    ;; (defun un-urlify (fname-or-url)
                                    ;;   "A trivial function that replaces a prefix of file:/// with just /."
                                    ;;   (if (string= (substring fname-or-url 0 8) "file:///")
                                    ;;       (substring fname-or-url 7)
                                    ;;     fname-or-url))
                                    ;; (defun th-evince-sync (file linecol &rest ignored)
                                    ;;   (let* ((fname (url-unhex-string (un-urlify file)))
                                    ;;          (buf (find-buffer-visiting fname))
                                    ;;          (line (car linecol))
                                    ;;          (col (cadr linecol)))
                                    ;;     (if (null buf)
                                    ;;         (message "[Synctex]: %s is not opened..." fname)
                                    ;;       (switch-to-buffer buf)
                                    ;;       (goto-line (car linecol))
                                    ;;       (unless (= col -1)
                                    ;;         (move-to-column col)))))
                                    ;; (defvar *dbus-evince-signal* nil)
                                    ;; (defun enable-evince-sync ()
                                    ;;   (require 'dbus)  
                                    ;;   (when (and
                                    ;;          (eq window-system 'x)
                                    ;;          (fboundp 'dbus-register-signal))
                                    ;;     (if (not (getenv "DBUS_SESSION_BUS_ADDRESS"))
                                    ;; 	(let* ((output (shell-command-to-string "dbus-launch")))
                                    ;; 	  (string-match "DBUS_SESSION_BUS_ADDRESS=\\(.*\\)" output)
                                    ;; 	  (setenv "DBUS_SESSION_BUS_ADDRESS" (match-string 1 output))))))
                                    ;; (enable-evince-sync)
                                    ;; (unless *dbus-evince-signal*
                                    ;;   (setf *dbus-evince-signal*
                                    ;; 	(dbus-register-signal
                                    ;; 	 :session nil "/org/gnome/evince/Window/0"
                                    ;; 	 "org.gnome.evince.Window" "SyncSource"
                                    ;; 	 'th-evince-sync)))
                                    ;; (add-hook 'LaTeX-mode-hook 'enable-evince-sync)
                                    ))
		    reftex)
		  el-get-sources)))

(let ((new_path (expand-file-name (concat el-get-dir "/emacsmirror-rcp"))))
  (if (not (member new_path el-get-recipe-path))
      (setq el-get-recipe-path
	    (append el-get-recipe-path (list new_path)))))

(setq my-packages
      (mapcar 'el-get-as-symbol
	      (append '(;; auto-complete-clang auto-complete-etags auto-complete-extension
					    ;; mo-git-blame ;; magithub 
					    gist ruby-electric autopair haml-mode
					    rspec-mode sass-mode cssh switch-window vkill
                                            frame-fns frame-cmds
					    ;; tabulated-list
					    magit-svn
					    popup
                                            fuzzy pcache gh
					    logito
					    markdown-mode
                                            ;; ac-python
                                            ggtags
					    thesaurus lua-mode
					    xcscope sudo-save

                                            vlfi)
		      (mapcar 'el-get-source-name el-get-sources))))
(el-get 'sync my-packages)
(el-get 'wait)


(require 'cc-mode)

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(use-package projectile :ensure t)
(use-package treemacs :ensure t)
(use-package yasnippet :ensure t)
(use-package lsp-mode :ensure t)
(use-package hydra :ensure t)
(use-package company-lsp :ensure t)
(use-package lsp-ui :ensure t)
(use-package lsp-java :ensure t :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java :after (lsp-java))
(use-package lsp-java-treemacs :after (treemacs))

(defun lsp-goto-type-definition-interactive (name)
  (interactive "MGoto definition: ")
  (lsp-find-locations "textDocument/typeDefinition" name))

(define-key lsp-mode-map (kbd "C-i") 'lsp-ui-imenu)
(define-key lsp-mode-map (kbd "C-t") 'lsp-ui-find-workspace-symbol)


(defun project-fuzzy-search (name)
  (interactive "MFind file FUZZY: ")
  (project-ensure-current)
  (let ((matches (project-search-fuzzy (project-current) name)))
    (if matches
        (if (= 1 (length matches))
            (find-file (car matches))
          (progn
            (setq matches (mapcar (lambda (x) (list x)) matches))
            (let ((choice (completing-read "Choose: " matches nil nil nil)))
              (when choice
                (find-file choice)))))
      (message "No reasonable matches found."))))
