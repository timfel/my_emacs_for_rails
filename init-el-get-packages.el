(condition-case nil
    (when
	(load
	 (expand-file-name "~/.emacs.d/elpa/package.el"))
      (package-initialize))
  (error (let ((buffer (url-retrieve-synchronously
			"http://tromey.com/elpa/package-install.el")))
	   (save-excursion
	     (set-buffer buffer)
	     (goto-char (point-min))
	     (re-search-forward "^$" nil 'move)
	     (eval-region (point) (point-max))
	     (kill-buffer (current-buffer))
	     (when
		 (load
		  (expand-file-name "~/.emacs.d/elpa/package.el"))
	       (package-initialize))))))
(setq package-archives '("tromey" . "http://tromey.com/elpa/"))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp)))))


;; ;; CEDET hack
;; (defun inversion-test (p v)
;;   (string= v (symbol-value
;; 	      (intern-soft (concat (symbol-name p) "-version")))))
(require 'compile) ;; Needed for some reason or other. el-get fails for me, otherwise

(setq el-get-sources
      '((:name auto-complete
	       :after (progn
				   (require 'auto-complete-config)
				   (global-auto-complete-mode t)     ;; enable global-mode
				   (setq ac-auto-start nil)            ;; automatically start
				   (setq ac-dwim 3)                  ;; Do what i mean
				   (setq ac-override-local-map nil)  ;; don't override local map
				   (ac-flyspell-workaround)
				   (setq ac-delay 5)
				   (setq ac-auto-show-menu 1)
				   (set-default 'ac-sources '(ac-source-yasnippet ac-source-semantic ac-source-abbrev ac-source-words-in-buffer))

				   (setq ac-modes (append ac-modes '(eshell-mode)))
				   (global-set-key (kbd "M-?") 'auto-complete)
				   (add-hook 'emacs-lisp-mode-hook
					     (lambda ()
					       (setq ac-sources '(ac-source-yasnippet
								  ac-source-abbrev
								  ac-source-words-in-buffer
								  ac-source-symbols))))
				   (add-hook 'eshell-mode-hook
					     (lambda ()
					       (setq ac-sources '(ac-source-yasnippet
								  ac-source-abbrev
								  ac-source-files-in-current-dir
								  ac-source-words-in-buffer))))))

	(:name yasnippet
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

	;; (:name ecb
	;;        :load-path "."
	;;        :features ecb
	;;        :after (lambda () (progn
	;; 			   (global-ede-mode 1)
	;; 			   (semantic-mode t)
	;; 			   (setq semantic-load-turn-everything-on t)
	;; 			   (setq ecb-tip-of-the-day nil)
	;; 			   (setq ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
	;; 			   (if window-system
	;; 			       (if (>= (window-height) 16)
	;; 				   (progn
	;; 				     (ecb-activate)
	;; 				     (add-hook 'window-setup-hook 'ecb-redraw-layout t))
	;; 				 (message "Not activating ECB, window height to small"))
	;; 			     (message "Not activating ECB, not using a window system")))))

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

	;; (:name color-theme-solarized
	;;        :after (progn
	;; 		;; Color theme
	;; 		(color-theme-solarized-light)))

	;; (:name color-theme-sanityinc-solarized
	;;        :type github
	;;        :pkgname "purcell/color-theme-sanityinc-solarized"
	;;        :depends color-theme
	;;        :after (progn
	;; 		(require 'color-theme-sanityinc-solarized)
	;; 		(add-hook 'after-make-frame-functions
	;; 			  color-theme-sanityinc-solarized-light)))

	;; (:name color-theme-sanityinc
	;;        :after (progn
	;; 		(add-hook 'after-make-frame-functions
	;; 			  color-theme-sanityinc-light)))
	
	;; (:name solarized-emacs
	;;        :after (progn
	;; 		(load-theme 'solarized-light)))

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

	;; (:name rinari
	;;       :after (lambda () (setq rinari-tags-file-name "TAGS")))

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

	(:name rsense
	       :type git
	       :url "git://github.com/m2ym/rsense.git"
	       :prepare (progn (setq rsense-home (expand-file-name (concat el-get-dir "/rsense"))))
	       :features rsense
	       :load-path "etc")

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

	;; (:name dictionary-el    :type apt-get)
	;; (:name emacs-goodies-el :type apt-get)

	;; (:name showoff-mode
	;;        :type git
	;;        :url "https://github.com/developernotes/showoff-mode.git"
	;;        :load-path "."
	;;        :features showoff-mode)

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

	(:name fill-column-indicator
	       :after (progn
				   (setq fci-rule-column 81)
				   ;; (setq fci-always-use-textual-rule t)
				   ;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
				   ;; (global-fci-mode 1)
				   ))

	;; (:name emacs-evernote-mode
	;;        :description "Emacs Evernote Mode"
	;;        :type git
	;;        :url "https://github.com/awasira/emacs-evernote-mode.git"
	;;        :load-path "."
	;;        :features evernote-mode
	;;        :compile "emacs-evernote-mode.el"
	;;        :build `("ruby ruby/setup.rb")
	;;        :after (progn
	;; 		(setq evernote-username "timfelgentreff") ; optional: you can use this username as default.
	;; 		(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
	;; 		(add-to-list 'helm-sources anything-c-source-evernote-title)
	;; 		(defalias 'evernote-find 'anything-evernote-title)))

	(:name emacsmirror-rcp
	       :type git
	       :url "https://github.com/edenc/emacsmirror-rcp")

	;; (:name ajc-java-complete
	;;        :depends (emacsmirror-rcp)
	;;        :after (lambda () (progn
	;; 			   (require 'ajc-java-complete)
	;; 			   (require 'ajc-java-complete-config)
	;; 			   (add-hook 'java-mode-hook
	;; 				     (lambda () (progn
	;; 						  (ajc-java-complete-mode t)
	;; 						  (setq ac-omni-completion-sources
	;; 							'((cons "\\.[A-Za-z0-9_]*" '(ac-source-ajc-method))
	;; 							  (cons "\s[A-Z][A-Za-z]+" '(ac-source-ajc-class))
	;; 							  (cons "new [A-Za-z]+(" '(ac-source-ajc-constructor)) )) ))) )))

	;; (:name jdee
	;;        :type git
	;;        :url "https://github.com/timfel/jdee.git"
	;;        :build ("ant configure" "ant")
	;;        :load-path ("lisp")
	;;        :depends (ecb)
	;;        :after (lambda () (progn
	;; 			   			   (setq jde-auto-parse-enable nil)
	;; 			   			   (setq jde-enable-senator nil)
	;; 			   			   (setq defer-loading-jde t)
	;; 			   			   (if defer-loading-jde
	;; 			   			       (progn
	;; 			   			   	 (autoload 'jde-mode "jde" "JDE mode." t)
	;; 			   			   	 (setq auto-mode-alist
	;; 			   			   	       (append
	;; 			   			   		'(("\\.java\\'" . jde-mode))
	;; 			   			   		auto-mode-alist)))
	;; 			   			     (require 'jde))
	;; 			   			   (let ((java-dir
	;; 			   				  (substring
	;; 			   				   (shell-command-to-string "dirname $(dirname $(readlink -f $(which java)))")
	;; 			   				   0 -1)))
	;; 			   			     (setq
	;; 			   			      jde-web-browser "xdg-open"
	;; 			   			      jde-doc-dir (concat java-dir "/doc")
	;; 			   			      jde-sourcepath '( (expand-file-name "~/Devel/" ) )
	;; 			   			      jde-db-option-connect-socket '(nil "28380")
	;; 			   			      jde-jdk-registry (quote (("1.7" . java-dir)))
	;; 			   			      jde-jdk `("1.7")
	;; 			   			      )
	;; 			   			     )
	;; 			   			   ;; (require 'jdibug)
	;; 			   			   ;; (define-key jde-mode-map [f8]   'jdibug-step-over)
	;; 			   			   ;; (define-key jde-mode-map [M-f8] 'jdibug-step-into)
	;; 			   			   ;; (define-key jde-mode-map [f7]   'jdibug-step-out)
	;; 			   			   ;; (define-key jde-mode-map [M-f7] 'jdibug-resume)
	;; 			   			   (defun flymake-java-ecj-init ()
	;; 			   			     (let* ((temp-file   (flymake-init-create-temp-buffer-copy
	;; 			   			   			  'jde-ecj-create-temp-file))
	;; 			   			   	    (local-file  (file-relative-name
	;; 			   			   			  temp-file
	;; 			   			   			  (file-name-directory buffer-file-name))))
	;; 			   			       ;; Change your ecj.jar location here
	;; 			   			       (list "java" (list "-jar" "/usr/share/java/ecj.jar" "-Xemacs" "-d" "/dev/null"
	;; 			   			   			  "-source" "1.6" "-target" "1.6" "-proceedOnError"
	;; 			   			   			  "-classpath"
	;; 			   			   			  (jde-build-classpath jde-global-classpath) local-file))))
	;; 			   			   (defun flymake-java-ecj-cleanup ()
	;; 			   			     "Cleanup after `flymake-java-ecj-init' -- delete temp file and dirs."
	;; 			   			     (flymake-safe-delete-file flymake-temp-source-file-name)
	;; 			   			     (when flymake-temp-source-file-name
	;; 			   			       (flymake-safe-delete-directory (file-name-directory flymake-temp-source-file-name))))

	;; 			   			   (defun jde-ecj-create-temp-file (file-name prefix)
	;; 			   			     "Create the file FILE-NAME in a unique directory in the temp directory."
	;; 			   			     (file-truename (expand-file-name (file-name-nondirectory file-name)
	;; 			   			   				      (expand-file-name  (int-to-string (random)) (flymake-get-temp-dir)))))
	;; 			   			   (push '("\\(.*?\\):\\([0-9]+\\): error: \\(.*?\\)\n" 1 2 nil 2 3 (6 compilation-error-face))
	;; 			   			   	 compilation-error-regexp-alist)
	;; 			   			   (push '("\\(.*?\\):\\([0-9]+\\): warning: \\(.*?\\)\n" 1 2 nil 1 3 (6 compilation-warning-face))
	;; 			   			   	 compilation-error-regexp-alist)
	;; 			   )))
	))

;; Auctex depends on pdflatex being available, only install if desired on this system
(if (executable-find "pdflatex")
    (setq el-get-sources
	  (append '((:name auctex
			   :after (progn
					       (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
					       (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
					       (add-hook 'LaTeX-mode-hook 'reftex-mode)
					       ;; (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
					       (add-hook 'LaTeX-mode-hook 'flyspell-mode)
					       (add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\M-i" 'ispell-word)))
					       (setq reftex-plug-into-AUCTeX t)
					       (setq TeX-auto-save t)
					       (setq TeX-save-query nil)
					       (setq TeX-parse-self t)))
		    reftex)
		  el-get-sources)))

(let ((new_path (expand-file-name (concat el-get-dir "/emacsmirror-rcp"))))
  (if (not (member new_path el-get-recipe-path))
      (setq el-get-recipe-path
	    (append el-get-recipe-path (list new_path)))))

(setq my-packages
      (mapcar 'el-get-as-symbol
	      (append '(auto-complete-clang auto-complete-etags auto-complete-extension
					    mo-git-blame ;; magithub 
					    gist ruby-electric autopair haml-mode
					    rspec-mode sass-mode cssh switch-window vkill
                                            frame-fns frame-cmds
					    ;; tabulated-list
					    magit-svn
					    popup fuzzy pcache gh
					    logito
					    markdown-mode ac-python
					    thesaurus lua-mode
					    xcscope sudo-save
					    vlfi)
		      (mapcar 'el-get-source-name el-get-sources))))
(el-get 'sync my-packages)
(el-get 'wait)
